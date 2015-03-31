;;; popup-switcher.el --- switch to other buffers and files via popup. -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015  Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/popup-switcher
;; Keywords: popup, switch, buffers, functions
;; Version: 0.2.10
;; Package-Requires: ((cl-lib "0.3")(popup "0.5.2"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

(require 'cl-lib)
(require 'popup)
(require 'artist)
(require 'recentf)

(defcustom psw-in-window-center nil
  "Non-nil means horizontal locate popup menu in the window center.
Locate popup menu in the `fill-column' center otherwise."
  :type 'boolean)

(defcustom psw-use-flx nil
  "Non-nil enables `flx' fuzzy matching engine for isearch in popup menus."
  :type 'boolean)

(defcustom psw-popup-menu-max-length 15
  "Set maximum number of visible items in popup menus."
  :type 'integer)

(defcustom psw-mark-modified-buffers nil
  "Non-nil means mark modified buffers with star char (*)")

(defcustom psw-before-menu-hook nil
  "Hook runs before menu showed"
  :type 'hook)

(defcustom psw-after-switch-hook nil
  "Hook runs after buffer switch"
  :type 'hook)

(defun psw-window-line-number ()
  (save-excursion
    (goto-char (window-start))
    (line-number-at-pos)))

(defun psw-get-buffer-list ()
  (cl-remove-if (lambda (buf) (or (minibufferp buf)
                             (let ((buf-name (buffer-name buf)))
                               (and (>= (length buf-name) 2)
                                    (equal (substring buf-name 0 2) " *")))))
                (buffer-list)))

(defun psw-copy-face (old-face new-face)
  "Safe copy face to handle absence of `flx-highlight-face' if
`flx-ido' is not installed."
  (when psw-use-flx
    (if (facep old-face)
        (copy-face old-face new-face)
      (setq new-face nil))))

(defvar psw-buffer-modified t
  "Current buffer original modified state.")

(cl-defun psw-popup-menu (&key
                          item-names-list
                          fallback
                          (window-center nil))
  "Popup selection menu.
ITEM-NAMES-LIST - list of item names to select.
FALLBACK - popup loop unexpected key handler.
WINDOW-CENTER - if t, overrides `psw-in-window-center' var value."
  (if (equal (length item-names-list) 0)
      (error "Popup menu items list is empty."))
  (let* ((menu-height (min psw-popup-menu-max-length
                           (length item-names-list)
                           (- (window-height) 4)))
         (x (+ (/ (- (if (or psw-in-window-center window-center)
                         (window-width)
                       fill-column)
                     (apply 'max (mapcar 'length item-names-list)))
                  2)
               (window-hscroll)))
         (y (+ (- (psw-window-line-number) 2)
               (/ (- (window-height) menu-height) 2)))
         (modified (buffer-modified-p))
         (saved-text (buffer-substring (window-start) (window-end)))
         (old-pos (point))
         (inhibit-read-only t)
         (psw-temp-face (psw-copy-face 'flx-highlight-face 'psw-temp-face)))
    (setq psw-buffer-modified modified)
    (unwind-protect
        (progn
          (psw-copy-face 'popup-isearch-match 'flx-highlight-face)
          (let* ((menu-pos (save-excursion
                             (artist-move-to-xy x y)
                             (point)))
                 (target-item-name (popup-menu* item-names-list
                                                :point menu-pos
                                                :height menu-height
                                                :scroll-bar t
                                                :margin-left 1
                                                :margin-right 1
                                                :around nil
                                                :isearch t
                                                :fallback fallback)))
            target-item-name))
      (progn
        (when (buffer-modified-p)
          (delete-region (window-start) (window-end))
          (insert saved-text)
          (goto-char old-pos)
          (set-buffer-modified-p modified))
        (psw-copy-face 'psw-temp-face 'flx-highlight-face)))))

(defadvice popup-isearch-filter-list (around
                                      psw-popup-isearch-filter-list
                                      activate)
  "Choose between the regular popup-isearch-filter-list and flx-ido-match-internal"
  (if (and psw-use-flx
           (> (length pattern) 0))
      (if (not (require 'flx nil t))
          (progn
            ad-do-it
            (message "Please install flx.el and flx-ido.el if you use fuzzy completion"))
        (if (eq :too-big
                (catch :too-big
                  (setq ad-return-value (flx-ido-match-internal pattern list))))
            ad-do-it))
    ad-do-it))

(defun psw-nil? (x) (equal nil x))

(defun psw-zip (x y)
  (cl-mapcar #'list (setcdr (last x) x) y))

(defun psw-flatten (list-of-lists)
  (apply #'append list-of-lists))

(defun psw-compose (&rest funs)
  "Return function composed of FUNS."
  (let ((lex-funs funs))
    (lambda (&rest args)
      (cl-reduce 'funcall (butlast lex-funs)
                 :from-end t
                 :initial-value (apply (car (last lex-funs)) args)))))

(defun psw-get-plain-string (properties-string)
  "Remove text properties from the string."
  (format "%s" (intern properties-string)))

(cl-defun psw-get-item-by-name (&key item-names-list
                                     items-list
                                     target-item-name)
  "Return the item by it's name."
  (let ((items-map (psw-flatten (psw-zip item-names-list items-list))))
    (lax-plist-get items-map target-item-name)))

(cl-defun psw-switcher (&key
                        items-list
                        item-name-getter
                        switcher
                        (fallback 'popup-menu-fallback))
  "Simplify create new popup switchers.
ITEMS-LIST - the essence items list to select.
ITEM-NAME-GETTER - function to convert each item to it's text representation.
SWITCHER - function, that describes what do with the selected item."
  (run-hooks 'psw-before-menu-hook)
  (let ((item-names-list (mapcar
                          (lambda (x) (funcall
                                  (psw-compose 'psw-get-plain-string
                                               item-name-getter) x))
                          items-list)))
    (funcall switcher
             (psw-get-item-by-name
              :item-names-list item-names-list
              :items-list items-list
              :target-item-name (psw-popup-menu
                                 :item-names-list item-names-list
                                 :fallback fallback))))
  (run-hooks 'psw-after-switch-hook))

(cl-defun psw-is-temp-buffer (&optional buffer)
  "Find buffers with names bounded with stars like *Messages* or *scratch*."
  (with-current-buffer (or buffer (current-buffer))
    (let ((buffer-name-length (length (buffer-name))))
      (and
       (equal "*" (substring (buffer-name) 0 1))
       (equal "*" (substring (buffer-name)
                             (1- buffer-name-length)
                             buffer-name-length))))))

;;;###autoload
(defun psw-switch-buffer ()
  (interactive)
  (psw-switcher
   :items-list (psw-get-buffer-list)
   :item-name-getter (lambda (buffer)
                       (with-current-buffer buffer
                         (if (and psw-mark-modified-buffers
                                  (buffer-modified-p)
                                  (not (psw-is-temp-buffer)))
                             (concat (buffer-name) " *")
                           (buffer-name))))
   :switcher 'switch-to-buffer
   :fallback (lambda (key _)
               (if (or (equal (kbd "C-k") key)
                       (equal (kbd "C-d") key))
                   (let* ((menu (car popup-instances))
                          (buff (nth (popup-cursor menu)
                                     (popup-list menu)))
                          (same-buffer-p (when (and (equal
                                                     (buffer-name
                                                      (current-buffer))
                                                     buff))
                                           (set-buffer-modified-p
                                            psw-buffer-modified)
                                           t)))
                     (when (kill-buffer buff)
                       (setf (popup-list menu)
                             (remove buff (popup-list menu))
                             (popup-original-list menu)
                             (remove buff (popup-original-list menu)))
                       (if (not same-buffer-p)
                           (progn
                             (popup-previous menu)
                             (popup-draw menu))
                         (popup-delete menu))))))))

;;;###autoload
(defun psw-switch-recentf ()
  (interactive)
  (psw-switcher
   :items-list recentf-list
   :item-name-getter 'identity
   :switcher 'find-file))

;;;###autoload
(defun psw-switch-projectile-files ()
  (interactive)
  (psw-switcher
   :items-list (projectile-current-project-files)
   :item-name-getter 'identity
   :switcher (lambda (file)
               (find-file
                (expand-file-name file
                                  (projectile-project-root))))))

;;;###autoload
(defun psw-navigate-files (&optional start-path)
  (interactive)
  (let ((start-path (or start-path
                        (expand-file-name ".." (buffer-file-name)))))
    (psw-switcher
     :items-list (cl-remove-if
                  (lambda (path) (equal (file-name-nondirectory (car path)) "."))
                  (directory-files-and-attributes start-path t))
     :item-name-getter (psw-compose 'file-name-nondirectory 'car)
     :switcher (lambda (entity)
                 (let* ((entity-path (car entity))
                        (entity-name (file-name-nondirectory entity-path)))
                   (if (cadr entity) ; is a directory sign
                       ;; is a directory
                       (psw-navigate-files
                        (expand-file-name entity-name start-path))
                     ;; is a file
                     (find-file entity-path)))))))

(eval-after-load "eassist"
  '(progn
     ;;
     (defun psw-eassist-list-parser (method-tags)
       "Return list of pairs: first - function name, second - it's position."
       (let ((method-triplets (mapcar
                               'eassist-function-string-triplet method-tags)))
         (cl-mapcar '(lambda (name position)
                       (list name position))
                    (mapcar 'caddr method-triplets)
                    (mapcar 'semantic-tag-start method-tags))))
     ;;
     ;; TODO: use imenu for emacs lisp
     (defun psw-imenu-list-parser (tags)
       "Simplify list of pairs for `imenu--index-alist'."
       (cl-remove-if
        'psw-nil?
        (loop for tag in tags
              collect (if (and (listp tag)
                               (not (equal imenu--rescan-item tag)))
                          (list (car tag)
                                (let ((pos-info (cdr tag)))
                                  (cond ((numberp pos-info) pos-info)
                                        ((markerp pos-info) pos-info)
                                        ((overlayp pos-info)
                                         (overlay-start pos-info)))))))))
     ;;
     (defun psw-get-tags-list ()
       (let ((eassist-list (psw-eassist-list-parser (eassist-function-tags))))
         (if eassist-list eassist-list
           (psw-imenu-list-parser (or imenu--index-alist
                                      (imenu--make-index-alist))))))
     ;;;###autoload
     (defun psw-switch-function ()
       (interactive)
       (setq eassist-buffer (current-buffer))
       (setq eassist-current-tag (semantic-current-tag))
       (psw-switcher
        :items-list (psw-get-tags-list)
        :item-name-getter 'car
        :switcher (psw-compose 'goto-char 'cadr)))))

(provide 'popup-switcher)

;;; popup-switcher.el ends here
