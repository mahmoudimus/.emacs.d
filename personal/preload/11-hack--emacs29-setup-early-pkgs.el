;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-

;; Make all commands of the “package” module present.
(require 'package)
;; Add MELPA to package sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Prefer GNU over MELPA (optional)
(setq package-archive-priorities '(("gnu" . 20)("melpa" . 10)))
(package-initialize)

;; ensure 'use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(if (package-installed-p 'use-package)
    (progn
      (use-package s :ensure t)
      (use-package flycheck
        :ensure t
        ;; :diminish
        ;; :autoload flycheck-redefine-standard-error-levels
        ;; :hook (after-init . global-flycheck-mode)
        :init (
               ;; flycheck-emacs-lisp-load-path 'inherit
               ;; flycheck-indication-mode (if (display-graphic-p)
               ;;                              'right-fringe
               ;;                            'right-margin)
               ;; ;; Only check while saving and opening files
               ;; flycheck-check-syntax-automatically '(save mode-enabled)
               setq flycheck-global-modes
               '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                     org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)))
      (use-package dash :ensure t)
      (message "** ✅(OK): Successfully installed: s, flycheck and dash")
      t)
  (message "** ❌(FAIL): Failed to install s and dash"))
