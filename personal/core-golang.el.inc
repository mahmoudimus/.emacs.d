;;; -*- mode: emacs-lisp; lexical-binding: t-*-
;;; core-golang.el
;;
;; Copyright (c) 2020 -- Mahmoud Abdelkader
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
(use-package lsp-mode
  :ensure t
  ;; uncomment to enable gopls http debug server
  ;; :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:0"))
  :commands (lsp lsp-deferred)
  :config (progn
            ;; use flycheck, not flymake
            (setq lsp-prefer-flymake nil))
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; optional - provides fancy overlay information
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            ;; ;; disable inline documentation
            ;; (setq lsp-ui-sideline-enable nil)
            ;; ;; disable showing docs on hover at the top of the window
            ;; (setq lsp-ui-doc-enable nil)
            )
  )

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  ;; Optionally enable completion-as-you-type behavior.
  :config (progn
            ;; ;; don't add any dely before trying to complete thing being typed
            ;; ;; the call/response to gopls is asynchronous so this should have little
            ;; ;; to no affect on edit latency
            ;; (setq company-idle-delay 0)
            ;; ;; start completing after a single character instead of 3
            ;; (setq company-minimum-prefix-length 1)
            ;; ;; align fields in completions
            ;; (setq company-tooltip-align-annotations t)
            )
  )

;; optional package to get the error squiggles as you edit
(use-package flycheck
  :ensure t)

;; if you use company-mode for completion (otherwise, complete-at-point works out of the box):
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; (lsp-register-custom-settings
;;  '(("gopls.completeUnimported" t t)
;;    ("gopls.staticcheck" t t)))

(defun go-run-tests (args)
  (save-selected-window
    (async-shell-command (concat "go test -v " args))))

(defun go-run-package-tests ()
  (interactive)
  (go-run-tests ""))

(defun go-run-package-tests-nested ()
  (interactive)
  (go-run-tests "./..."))

(defun go-run-main ()
  (interactive)
  ;; (save-buffer)
  (save-selected-window
    (async-shell-command
     (format "go run %s"
             (shell-quote-argument (buffer-file-name))))))

(defun go-rerun ()
  (interactive)
  (compile compile-command))

;; "go build -v -o ~/.go-out; and go test -v; and go vet; and ~/.go-out"))
(defun go-build ()
  (interactive)
  (compile (concat "go build " (buffer-file-name))))

(defun go-test ()
  (interactive)
  (compile (concat "go test -v" (buffer-file-name))))

(defun go-run ()
  (interactive)
  (compile (concat "go run " (buffer-file-name))))

(defun mahmoudimus/go-mode-hook ()
  (message "go-mode-hook")
  ;; Use goimports instead of go-fmt
  ;; (setq gofmt-command "goimports")
  ;; ;; Call Gofmt before saving
  ;; (add-hook 'before-save-hook 'gofmt-before-save)

  (setq compilation-read-command nil)
  (cond ((fboundp 'spacemacs/go-run-main)
         (local-set-key (kbd "C-c C-r") 'spacemacs/go-run-main))
        (t (local-set-key (kbd "C-c C-r") 'go-run-main)))

  (local-set-key (kbd "s-i s-i") 'go-rerun)
  (local-set-key (kbd "s-i C-f") 'go-run)
  (local-set-key (kbd "s-i C-t") 'go-test)
  (local-set-key (kbd "s-i C-b") 'go-build))

(add-hook 'go-mode-hook 'mahmoudimus/go-mode-hook)
(add-hook 'go-mode-hook 'mahmoudimus/lsp-connect)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'core-golang)
