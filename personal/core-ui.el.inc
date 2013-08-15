;;; -*- mode: lisp -*-
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

;; removing the menu under OS X doesn't make much sense
;; since esk already removes it for us
(when (eq system-type 'darwin)
  (menu-bar-mode t))

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
;; god this is fucking annoying!!
(setq scroll-step 1)

;; add the theme path for theme lookup
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
;; I want to customize my theme.
;; it's safe, so use the NO-CONFIRM flag
;; http://stackoverflow.com/questions/8545756/how-to-treat-solarized-as-a-safe-theme
;; (load-theme 'zenburn t)
;; (load-theme 'tomorrow-night-bright t)
;; global hl mode
(global-hl-line-mode 1)

;; flymake cursor support so i don't have to use my mouse
;; to get the error message.
(require 'flymake-cursor)


;; Escreen support
;; escreen is simpler than elscreen...but elscreen spawns new 0-9
;; set of screens for a new frame whereas escreen does not
;; it is better than elscreen because:
;;     elscreen has issues when using with emacsclient -c
;;     (does not always create new frame, messes up existing frame);
;; in escreen, each screen has its own ring of recently visited files (good!)
;; http://blog.tapoueh.org/news.dim.html#%20Escreen%20integration
;; http://www.splode.com/~friedman/software/emacs-lisp/#ui
(load "escreen")
(escreen-install)

;; add C-\ l to list screens with emphase for current one
(defun escreen-get-active-screen-numbers-with-emphasis ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
    (emphased ""))

    (dolist (s escreens)
      (setq emphased
        (concat emphased (if (= escreen-current-screen-number s)
                 (propertize (number-to-string s)
                         ;;'face 'custom-variable-tag) " ")
                         ;; 'face 'info-title-3)
                         'face 'font-lock-warning-face)
                   (number-to-string s))
            " ")))
    (message "escreen: active screens: %s" emphased)))

;;
;; We want the last/prev/next escreen function to show the list with
;; emphasis
;;
(defadvice escreen-goto-last-screen
  (after escreen-goto-last-screen activate)
  "Show the escreen list each time we go to last screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-prev-screen
  (after escreen-goto-prev-screen activate)
  "Show the escreen list each time we go to previous screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-goto-next-screen
  (after escreen-goto-next-screen activate)
  "Show the escreen list each time we go to next screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(defadvice escreen-create-screen
  (after escreen-create-screen activate)
  "Show the escreen list each time we create a new screen."
  (escreen-get-active-screen-numbers-with-emphasis))

(add-hook 'escreen-goto-screen-hook 'escreen-get-active-screen-numbers-with-emphasis)

(provide 'core-ui)
