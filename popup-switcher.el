;;; popup-switcher.el --- switch to other buffers and files via popup.

;; Copyright (C) 2013  Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/popup-switcher
;; Keywords: popup, switch, buffers, functions
;; Version: 0.2.2
;; Package-Requires: ((popup "0.5.0"))

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

(eval-when-compile (require 'cl))

(require 'popup)
(require 'artist)
(require 'recentf)

(defvar psw-in-window-center nil
  "Non-nil means horizontal locate popup menu in the window center.
Locate popup menu in the `fill-column' center otherwise.")

(defcustom psw-after-switch-hook nil
  "Hook runs after buffer switch")

(defun psw-window-line-number ()
  (save-excursion
    (goto-char (window-start))
    (line-number-at-pos)))

(defun psw-get-buffer-list ()
  (remove-if (lambda (buf) (or (minibufferp buf)
                               (equal (substring (buffer-name buf) 0 2) " *")))
             (buffer-list)))

(defun psw-popup-menu (item-names-list &optional window-center)
  "Popup selection menu.
`item-names-list' - list of item names to select.
`psw-in-window-center' - if t, overrides `psw-in-window-center' var value."
  (if (equal (length item-names-list) 0)
      (error "Popup menu items list is empty."))
  (let* ((menu-height (min 15 (length item-names-list) (- (window-height) 4)))
         (x (/ (- (if (or psw-in-window-center window-center)
                      (window-width)
                    fill-column)
                  (apply 'max (mapcar 'length item-names-list))) 2))
         (y (+ (- (psw-window-line-number) 2)
               (/ (- (window-height) menu-height) 2)))
         (modified (buffer-modified-p))
         (saved-text (buffer-substring (window-start) (window-end)))
         (old-pos (point))
         (inhibit-read-only t))
    (unwind-protect
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
                                              :isearch t)))
          target-item-name)
      (when (buffer-modified-p)
        (delete-region (window-start) (window-end))
        (insert saved-text)
        (goto-char old-pos)
        (set-buffer-modified-p modified)))))


(defun psw-nil? (x) (equal nil x))

(defun psw-zip (x y)
  (mapcar* #'list (setcdr (last x) x) y))

(defun psw-flatten (list-of-lists)
  (apply #'append list-of-lists))

(defun psw-compose (&rest funs)
  "Return function composed of FUNS."
  (lexical-let ((lex-funs funs))
    (lambda (&rest args)
      (reduce 'funcall (butlast lex-funs)
              :from-end t
              :initial-value (apply (car (last lex-funs)) args)))))

(defun psw-get-plain-string (properties-string)
  "Remove text properties from the string."
  (format "%s" (intern properties-string)))

(defun* psw-get-item-by-name (&key item-names-list items-list target-item-name)
  "Return the item by it's name."
  (let ((items-map (psw-flatten (psw-zip item-names-list items-list))))
    (lax-plist-get items-map target-item-name)))

(defun* psw-switcher (&key
                      items-list
                      item-name-getter
                      switcher)
  "Simplify create new popup switchers.
`items-list' - the essence items list to select.
`item-name-getter' - function to convert each item to it's text representation.
`switcher' - function, that describes what do with the selected item."
  (let ((item-names-list (mapcar
                          (lambda (x) (funcall
                                       (psw-compose 'psw-get-plain-string
                                                item-name-getter) x))
                          items-list)))
    (funcall switcher
             (psw-get-item-by-name
              :item-names-list item-names-list
              :items-list items-list
              :target-item-name (psw-popup-menu item-names-list))))
  (run-hooks 'psw-after-switch-hook))

;;;###autoload
(defun psw-switch-buffer ()
  (interactive)
  (psw-switcher
   :items-list (psw-get-buffer-list)
   :item-name-getter 'buffer-name
   :switcher 'switch-to-buffer))

;;;###autoload
(defun psw-switch-recentf ()
  (interactive)
  (psw-switcher
   :items-list recentf-list
   :item-name-getter 'identity
   :switcher 'find-file))

(eval-after-load "eassist"
  '(progn
     ;;
     (defun psw-eassist-list-parser (method-tags)
       "Return list of pairs: first - function name, second - it's position."
       (let ((method-triplets (mapcar
                               'eassist-function-string-triplet method-tags)))
         (mapcar* '(lambda (name position)
                     (list name position))
                  (mapcar 'caddr method-triplets)
                  (mapcar 'semantic-tag-start method-tags))))
     ;;
     ;; TODO: use imenu for emacs lisp
     (defun psw-imenu-list-parser (tags)
       "Simplify list of pairs for `imenu--index-alist'."
       (remove-if
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
           (psw-imenu-list-parser (imenu--make-index-alist)))))
     ;;
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
