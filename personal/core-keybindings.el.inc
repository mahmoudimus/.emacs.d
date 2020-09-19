;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
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

(require 'core-packages)

;; font-sizing
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)

;; comments
;; https://gist.github.com/mahmoudimus/54eef83c4ccff660794aea5e6b7d0a64
(global-set-key (kbd "s-;") 'comment-dwim)

;; helm complete the file name with a tab

(require 'helm)
(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
(define-key helm-map (kbd "\t") 'helm-execute-persistent-action)

;; (global-set-key (kbd "s-/") 'comment-region)
;; (global-set-key (kbd "s-?") 'uncomment-region)
;; (global-set-key (kbd "s-[") 'ew-outdent)
;; (global-set-key (kbd "s-]") 'ew-indent)

;; yasnippet
(global-set-key (kbd "C-c e") 'yas-expand) ;; expand at key

;; dumbjump!
(global-set-key (kbd "M-g d") 'dumb-jump-go)

;; we're using emacs-mac, so we shoud use the tab window
(mac-toggle-tab-bar)

(setq mac-frame-tabbing nil)

(defun mahmoudimus-mac-new-tab ()
  "Create a new tab on mac"
  (interactive)
  (let ((mac-frame-tabbing t))
    (make-frame-command)))

(defun mahmoudimus-mac-kill-tab ()
  "Kill a tab (Just an alias for delete-frame since a tab on
mac is actually just a frame)"
  (interactive)
  (delete-frame))

(global-set-key (kbd "s-}") 'mac-next-tab)
(global-set-key (kbd "s-{") 'mac-previous-tab)

(define-key
  ;; create a new tab
  (current-global-map)
  (kbd "s-t")
  'mahmoudimus-mac-new-tab)

(define-key
  ;; close current tab
  (current-global-map)
  (kbd "s-w")
  'mahmoudimus-mac-kill-tab)

(define-key
  ;; save file
  (current-global-map)
  (kbd "s-s")
  'save-buffer)

(define-key
  ;; copy
  (current-global-map)
  (kbd "s-c")
  'evil-yank)

(define-key
  ;; paste
  (current-global-map)
  (kbd "s-v")
  'evil-paste-after)

(define-key
  ;; new frame
  (current-global-map)
  (kbd "s-n")
  'make-frame)

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
(global-set-key (kbd "C-o") 'evil-open-above)
(global-set-key (kbd "C-S-o") 'evil-open-below)

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

;; helm mode and recentf make me so happy in the god damn pants.
;; https://emacs.stackexchange.com/questions/14079/how-to-see-recently-opened-files-in-helm/14104#14104
;; https://www.reddit.com/r/emacs/comments/4wi7hz/how_do_you_go_through_directories_with_helm/
(global-unset-key (kbd "C-c f"))
(global-unset-key (kbd "C-x f"))

(global-set-key (kbd "C-c f")   'set-fill-column)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x f") 'helm-recentf)
(global-set-key (kbd "C-x s-f") 'helm-projectile-find-file)

;; https://github.com/emacs-helm/helm/wiki/Fuzzy-matching


;; Bind keys to make recording/recalling macros easier
(global-set-key (read-kbd-macro "A-M-m")
                'start-kbd-macro)

(global-set-key (read-kbd-macro "A-M-x")
                'call-last-kbd-macro)

;; Bind keys to allow for getting to the start of the next word
;; sort of like vi's 'w' in command-mode
(global-set-key (kbd "s-F") '(lambda ()
                               (interactive)
                               (forward-word 2)
                               (backward-word 1)))

;; make C-N forward-paragraph and C-P backward-paragraph
(global-set-key (kbd "s-N") 'forward-paragraph)
(global-set-key (kbd "s-P") 'backward-paragraph)

(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-b") 'backward-word)

;; No tabs damnit.
(setq-default indent-tabs-mode nil)

;; Make tabs stand out
(defface extra-whitespace-face
   '((t (:background "pale green")))
   "Used for tabs and such.")
(defvar my-extra-keywords
   '(("\t" . 'extra-whitespace-face)))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(add-hook 'text-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(add-hook 'python-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(font-lock-add-keywords nil my-extra-keywords)


;; Terminals
;;; Terminal setup function for 'screen' terminal.  There is a similar
;;; function for each terminal, like xterm and rvxt. The one for
;;; screen is missing, but it is called by multi-tty emacs if present.
(defun terminal-init-screen ()
  "Terminal initialization function for screen-256color."
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

(when (is-in-terminal)
  (defun myfix-xterm  ()
    "Some key combinations would not work on `xterm' (e.g. shift-up mapped to <select>). This will fix it."
    (interactive)
    ;; (define-key input-decode-map "\e[1;2A" [S-up])
    ;; Fix XTerm issues when in terminals
    ;; input decode to fix C-return on terminals
    (define-key input-decode-map "\e[1;2V" [C-return])
    )


  (defun myfix-screen ()
    "Some key combinations would not work in `screen'. This will fix it."
    (interactive)
    ;; (define-key input-decode-map "\e[1;2A" [S-up])
    ;; (define-key input-decode-map "\e[1;2B" [S-down])
    ;; (define-key input-decode-map "\e[1;2C" [S-right])
    ;; (define-key input-decode-map "\e[1;2D" [S-left])
    ;; (define-key input-decode-map "\e[1;4A" [M-S-up])
    ;; (define-key input-decode-map "\e[1;4B" [M-S-down])
    ;; (define-key input-decode-map "\e[1;4C" [M-S-right])
    ;; (define-key input-decode-map "\e[1;4D" [M-S-left])
    ;; (define-key input-decode-map "\e[1;7A" [C-M-up])
    ;; (define-key input-decode-map "\e[1;7B" [C-M-down])
    ;; (define-key input-decode-map "\e[1;7C" [C-M-S-right])
    ;; (define-key input-decode-map "\e[1;7D" [C-M-left])
    ;; Fix XTerm issues when in terminals
    ;; input decode to fix C-return on terminals
    (define-key input-decode-map "\e[1;2V" [C-return])
    )


  ;;; Run the appropriate fix if emacs is run directly from terminal.
  (cond
    ((equal "xterm" (tty-type)) (myfix-xterm))
    ((equal "xterm-256color" (tty-type)) (myfix-xterm))
    ((equal "screen" (tty-type)) (myfix-screen))
    ((equal "screen-256color" (tty-type)) (myfix-screen))
    ((equal "screen-256color-bce" (tty-type)) (myfix-screen))
    )

  ;;; Run the appropriate fix if emacs is a multi-tty instance. The fix
  ;;; is run by emacs itself after tty initialization.

  ;; xterm
  (defadvice terminal-init-xterm (after myfix-xterm activate)
    "Map some terminal escape sequences to the correct keys"
    (myfix-xterm))
  ;; screen
  (defadvice terminal-init-screen (after myfix-screen activate)
    "Map some terminal escape sequences to the correct keys"
    (myfix-screen))

  )

(provide 'core-keybindings)
