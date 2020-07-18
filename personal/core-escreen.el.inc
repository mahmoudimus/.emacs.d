;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; core-escreen.el
;;
;; Copyright (c) 2011 -- Mahmoud Abdelkader
;;
;; Author: Mahmoud Abdelkader <mahmoud@linux.com>
;; URL: http://github.com/mahmoudimus/emacs.d/
;; Version: 1.0.0
;; Keywords: convenience, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file configures escreen support and its various
;; keybindings within my emacs configuration.

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

(require 'core-packages)

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

;; Escreen keybindings
;; http://www.macs.hw.ac.uk/~hwloidl/cool-el.html
;; easier keybindings if you like mouse support:
;; http://tapoueh.org/blog/2009/09/22-escreen-integration.html
;; Custom escreen keys
;;
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

;; hooks
(add-hook 'escreen-goto-screen-hook 'escreen-get-active-screen-numbers-with-emphasis)


(setq escreen-prefix-char (kbd "C-z"))
(global-set-key escreen-prefix-char 'escreen-prefix)
(global-set-key (kbd "C-\\ l") 'escreen-get-active-screen-numbers-with-emphasis)
(define-key escreen-map escreen-prefix-char 'escreen-goto-last-screen)

(global-set-key (kbd "s-{") 'escreen-goto-prev-screen)
(global-set-key (kbd "s-}") 'escreen-goto-next-screen)

;; add support for C-\ from terms
(require 'term)
(define-key term-raw-map escreen-prefix-char escreen-map)
(define-key term-raw-map (kbd "M-[") 'escreen-goto-prev-screen)
(define-key term-raw-map (kbd "M-]") 'escreen-goto-next-screen)

;; easier direct access to screens
(progn                  ; easier to C-M-x the block
  (global-set-key (kbd "C-M-0") 'escreen-goto-screen-0)
  (global-set-key (kbd "C-M-1") 'escreen-goto-screen-1)
  (global-set-key (kbd "C-M-2") 'escreen-goto-screen-2)
  (global-set-key (kbd "C-M-3") 'escreen-goto-screen-3)
  (global-set-key (kbd "C-M-4") 'escreen-goto-screen-4)
  (global-set-key (kbd "C-M-5") 'escreen-goto-screen-5)
  (global-set-key (kbd "C-M-6") 'escreen-goto-screen-6)
  (global-set-key (kbd "C-M-7") 'escreen-goto-screen-7)
  (global-set-key (kbd "C-M-8") 'escreen-goto-screen-8)
  (global-set-key (kbd "C-M-9") 'escreen-goto-screen-9))

(provide 'core-escreen)
