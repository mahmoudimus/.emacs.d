;;; core-ui.el
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

;; customize
(defgroup editor nil
  "Emacs Core Editor enhancements"
  :group 'core)

(require 'smart-operator)

;; Open Next Line
(require 'open-next-line)

;; ------------------------------------
;; require browse-kill-ring
;; ------------------------------------
(require 'browse-kill-ring)


;; Auto Completion
(add-to-list 'load-path (concat core-vendor-dir "auto-complete.el"))
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat core-vendor-dir "auto-complete.el/dict"))
(setq ac-auto-start 2)
(setq ac-dwim t)
(global-auto-complete-mode t)
(ac-config-default)

;; yasnippet
;; load all el files in the snippets directory, they're usually lisp
;; helpers that help with snippet expansions.
(setq core-custom-snippets (concat expanded-user-emacs-directory "snippets"))
(mapc 'load (directory-files core-custom-snippets t "^[^#].*el$"))
(require 'yasnippet)
(push core-custom-snippets yas/root-directory)
;; Load the snippets
(mapc 'yas/load-directory yas/root-directory)
(yas/global-mode t)

;; code borrowed from http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the
original" (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))


;; Mark whole line
(defun mark-line (&optional arg)
  "Marks a line"
  (interactive "p")
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

; code copied from http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

; patches by balle
; http://www.datenterrorist.de
(defun balle-python-shift-left ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
       mark-active)
    (setq start (region-beginning) end (region-end))
      (progn
    (setq bds (bounds-of-thing-at-point 'line))
    (setq start (car bds) end (cdr bds))))
  (python-indent-shift-left start end))
  (setq deactivate-mark nil)
)

(defun balle-python-shift-right ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
       mark-active)
    (setq start (region-beginning) end (region-end))
      (progn
    (setq bds (bounds-of-thing-at-point 'line))
    (setq start (car bds) end (cdr bds))))
  (python-indent-shift-right start end))
  (setq deactivate-mark nil)
)


(provide 'core-editor)
