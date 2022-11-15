;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
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
(require 'use-package)

;; Tab size to 4
(setq-default tab-width 4)

;; removing the menu under OS X doesn't make much sense
;; since esk already removes it for us
(when (eq system-type 'darwin)
  (menu-bar-mode t))

;; nice scrolling
(setq scroll-margin 0)
(when (boundp 'mac-carbon-version-string)
  ;; we're using emacs-mac, so we shoud set the scroll stuff
  (setq scroll-conservatively 100000
        scroll-preserve-screen-position 1))

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
;; god this is fucking annoying!!
(setq scroll-step 1)

;; install all-the-icons!
(defun is-in-terminal()
  (not (display-graphic-p)))

(use-package all-the-icons
  :if (display-graphic-p))

;; add the theme path for theme lookup
(add-to-list 'custom-theme-load-path (concat spacemacs-dir "themes/"))
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; I want to customize my theme.
  ;; it's safe, so use the NO-CONFIRM flag
  ;; http://stackoverflow.com/questions/8545756/how-to-treat-solarized-as-a-safe-theme
  ;; (load-theme 'zenburn t)
  ;; (load-theme 'tomorrow-night-bright t)
  ;; get rid of "Loading a theme can run Lisp code. Really load? (y or n) " message
  ;; (load-theme 'spacemacs-light t)
  (load-theme 'doom-snazzy t))

;; global hl mode
(global-hl-line-mode 1)

;; change regexp highlight face to something other than yellow
(defface highlight-regexp-face
  '((t (:foreground "#B28BD6")))
  "face for highlight-regexp")

;; http://stackoverflow.com/a/268205/133514
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

;; neotree
;; https://github.com/jaypei/emacs-neotree/issues/164
;; (defun neotree-startup ()
;;   (interactive)
;;   (neotree-show)
;;   (call-interactively 'other-window))

;; (if (daemonp)
;;     (add-hook 'server-switch-hook
;;               (lambda () (call-interactively 'neotree-show)))
;;     ;; (add-hook 'server-switch-hook #'neotree-startup)
;;   (add-hook 'after-init-hook #'neotree-toggle)
;;   ;; (add-hook 'after-init-hook #'neotree-startup)
;;   )
;; (add-hook 'window-setup-hook #'neotree-toggle)
;; (global-set-key (kbd "M-1") 'neotree-toggle)

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-1") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-width                         30
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-sorting                       'alphabetic-asc
          treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-1"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
;;   :after treemacs persp-mode ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))


;; https://github.com/deb0ch/emacs-winum#keybindings
(winum-set-keymap-prefix (kbd "C-c"))

;; if bound with karabiner, left-control is bound to M-s +shift modifier


;; emacs neotree go to directory
(defadvice helm-projectile-find-file
    (after helm-projectile-find-file activate)
  (neotree-dir default-directory))

(provide 'core-ui)
