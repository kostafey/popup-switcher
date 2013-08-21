;;; popup-switcher.el --- switch to other buffers and files via popup.

;; Copyright (C) 2013  Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/popup-switcher
;; Keywords: popup, switch, buffers, functions
;; Version: 0.2
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

(require 'popup)
(require 'artist)

(defvar psw-in-window-center nil
  "Non-nil means horizontal locate popup menu in the window center.
Locate popup menu in the `fill-column' center otherwise.")

(defcustom psw-after-switch-hook nil
  "Hook runs after buffer switch")

(defun psw-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun psw-window-line-number ()
  (save-excursion
    (goto-char (window-start))
    (line-number-at-pos)))

(defun psw-get-buffer-list ()
  (psw-filter (lambda (a) (and
                      (buffer-live-p a)
                      (not (minibufferp a))
                      (not (equal (substring (buffer-name a) 0 2) " *"))))
              (buffer-list)))

;; (psw-popup-menu (mapcar (lambda(a) (format "%s" (intern (buffer-name a))))
;;                         (psw-get-buffer-list)))
;; (remove-text-properties start end '(face nil))
;; (concat  (intern (buffer-name (current-buffer))))

(defun psw-popup-menu (item-names-list &optional window-center)
  "Popup selection menu.
`selection-list' - list of items to select.
`item-name-getter' - function for item to string conversion.
`psw-in-window-center' - if t, overrides `psw-in-window-center' var value."
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


(defun psw-get-plain-string (properties-string)
  (format "%s" (intern properties-string)))

(defun zip (x y)
  (mapcar* #'list (setcdr (last x) x) y))

(defun flatten (list-of-lists)
  (apply #'append list-of-lists))

(defun* psw-get-item-by-name (&key item-names-list items-list target-item-name)
  (let ((items-map (flatten (zip item-names-list items-list))))
    ;; (mapc (lambda (a)
    ;;         (setq items-map
    ;;               (lax-plist-put items-map (funcall item-name-getter a) a)))
    ;;       items-list)
    (lax-plist-get items-map target-item-name)))

(defun psw-switch-buffer ()
  (interactive)
  (switch-to-buffer
   (psw-popup-menu (mapcar
                    (lambda (b) (psw-get-plain-string (buffer-name b)))
                    (psw-get-buffer-list))))
  (run-hooks 'psw-after-switch-hook))

(defun psw-switch-recentf ()
  (interactive)
  (find-file
   (psw-popup-menu (mapcar 'psw-get-plain-string recentf-list)))
  (run-hooks 'psw-after-switch-hook))

(eval-after-load "eassist"
  '(progn
     ;;
     (defun psw-eassist-list-parser (eassist-tags)
       "Return list of pairs: first - function name, second - it's position."
       (let* ((method-tags (eassist-function-tags))
              (method-triplets (mapcar
                                'eassist-function-string-triplet method-tags)))
         (mapcar* '(lambda (name position)
                     (list name position))
                  (mapcar 'caddr method-triplets)
                  (mapcar 'semantic-tag-start method-tags))))
     ;;
     (defun psw-switch-function ()
       (interactive)
       (setq eassist-buffer (current-buffer))
       (setq eassist-current-tag (semantic-current-tag))
       (let* ((items-list (psw-eassist-list-parser (eassist-function-tags)))
              (item-names-list (mapcar
                                (lambda (a) (psw-get-plain-string (car a)))
                                items-list)))
         (goto-char
          (cadr
           (psw-get-item-by-name
            :item-names-list item-names-list
            :items-list items-list
            :target-item-name (psw-popup-menu item-names-list)))))
       (run-hooks 'psw-after-switch-hook))))

(provide 'popup-switcher)
