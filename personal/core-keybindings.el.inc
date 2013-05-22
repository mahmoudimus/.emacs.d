;;; -*- mode: lisp -*-
;;; core-keybindings.el
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

;; Escreen keybindings
;; http://www.macs.hw.ac.uk/~hwloidl/cool-el.html
;; easier keybindings if you like mouse support:
;; http://tapoueh.org/blog/2009/09/22-escreen-integration.html
;; Custom escreen keys
;;

(require 'core-packages)
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


;; auto-complete keybindings
(define-key ac-mode-map (kbd "<C-tab>") 'auto-complete)
;; (define-key ac-mode-map (kbd "M-") 'auto-complete)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(setq ac-dwim t)

;; text keybindings

;; text moving
(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; text marking
(global-set-key (kbd "C-c l") 'mark-line)

;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)

;; duplicate a line and comment the first
(global-set-key (kbd "C-c c")(lambda()(interactive)(djcb-duplicate-line t)))

;; yes i know..inverse of vi but i'm so used to it.
(global-set-key (kbd "C-O") 'open-next-line)
(global-set-key (kbd "C-o") 'open-previous-line)

;; browse-kill-ring
(browse-kill-ring-default-keybindings)


;; move text up and down
;; (define-key python-mode-map (kbd "M-<right>") 'balle-python-shift-right)
;; (define-key python-mode-map (kbd "M-<left>") 'balle-python-shift-left)
;; (define-key python-mode-map "\C-ci" 'rope-auto-import)
;; (define-key python-mode-map "\C-c\C-d" 'rope-show-calltip)


;; prelude remapping

(global-unset-key (kbd "C-x p"))
(global-set-key (kbd "C-x P") 'proced)

;; ido mode and recentf make me so happy in the god damn pants.
(global-unset-key (kbd "C-c f"))
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-c C-f") 'prelude-recentf-ido-find-file)

(provide 'core-keybindings)
