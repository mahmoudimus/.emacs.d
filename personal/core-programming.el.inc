;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; core-programming.el
;;
;; Copyright (c) 2011 -- Mahmoud Abdelkader
;;
;; Author: Mahmoud Abdelkader <mahmoud@linux.com>
;; URL: http://github.com/mahmoudimus/emacs.d/
;; Version: 1.0.0
;; Keywords: convenience, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within my emacs configuration.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'imenu)

(defun core-ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;; show the name of the current function definition in the modeline
;;(require 'which-func)
;;(which-func-mode t)

(defun all(f lst)
  "Return non-nil if function f(list_element) returns non-nil for all
   the elements of the list"
  (let ((result t))
    (dolist (el lst)
      (setq result (and result (funcall f el))))
       result))


(defvar remove-tabs-inhibit-for-modes '()
  "Mode to not untabify")

(add-to-list 'remove-tabs-inhibit-for-modes 'makefile-mode)
(add-to-list 'remove-tabs-inhibit-for-modes 'BSDmakefile)
(add-to-list 'remove-tabs-inhibit-for-modes 'indent-tabs-mode)

(defun remove-tabs-trspaces-empltylines()
  "Clean the current buffer from trailing spaces and empty lines at the end of the file.
   Also untabify the buffer in major-mode not in list remove-tabs-inhibit-for-modes"
  (interactive)
  (message "Removing tab hook called")

  (when (or (not remove-tabs-inhibit-for-modes)
            (all '(lambda (e) (not (derived-mode-p e))) remove-tabs-inhibit-for-modes))
    (message "Cleaning the buffer from tabs, trailing whitespaces and empty lines")
    (untabify 0 (point-max))
    (delete-trailing-whitespace)
    (let ((current (point)))
      (goto-char (point-max))
      (delete-blank-lines)
      (goto-char current)
      ))
  )

;; if using prelude
(setq prelude-clean-whitespace-on-save nil)

(defun core-prog-mode-hook ()
  "Default coding hook, useful with any programming language."
  ;; (flyspell-prog-mode)
  ;; keep the whitespace decent all the time
  (add-hook 'before-save-hook 'remove-tabs-trspaces-empltylines)
  (setq column-number-mode t)
  )

;; in Emacs 24 programming major modes generally derive
;; from a common mode named prog-mode
(add-hook 'prog-mode-hook 'core-prog-mode-hook)

(provide 'core-programming)

;;; core-programming.el ends here
