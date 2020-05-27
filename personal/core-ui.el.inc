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

;; Tab size to 4
(setq-default tab-width 4)

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
(load-theme 'spacemacs-light)
(global-hl-line-mode 1)

;; change regexp highlight face to something other than yellow
(defface highlight-regexp-face
  '((t (:foreground "#B28BD6")))
  "face for highlight-regexp")

(defun is-in-terminal()
  (not (display-graphic-p)))

;; http://stackoverflow.com/a/268205/133514
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

;; https://github.com/jaypei/emacs-neotree/issues/164

;; (defun neotree-startup ()
;;   (interactive)
;;   (neotree-show)
;;   (call-interactively 'other-window))

(if (daemonp)
    (add-hook 'server-switch-hook
              (lambda () (call-interactively 'neotree-show)))
    ;; (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-toggle)
  ;; (add-hook 'after-init-hook #'neotree-startup)
  )
(add-hook 'window-setup-hook #'neotree-toggle)

;; https://github.com/deb0ch/emacs-winum#keybindings
(winum-set-keymap-prefix (kbd "C-c"))

;; if bound with karabiner, left-control is bound to M-s +shift modifier
(global-set-key (kbd "M-1") 'neotree-toggle)

;; emacs neotree go to directory
(defadvice helm-projectile-find-file
    (after helm-projectile-find-file activate)
  (neotree-dir default-directory))

(provide 'core-ui)
