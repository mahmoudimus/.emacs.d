;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-

;; ;; Make all commands of the “package” module present.
;; (require 'package)
;; ;; Add MELPA to package sources
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; ;; Prefer GNU over MELPA (optional)
;; (setq package-archive-priorities '(("gnu" . 20)("melpa" . 10)))
;; (package-initialize)

;; ;; ensure 'use-package
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (if (package-installed-p 'use-package)
;;     (progn
;;       (use-package s :ensure t)
;;       (use-package flycheck
;;         :ensure t
;;         ;; :diminish
;;         ;; :autoload flycheck-redefine-standard-error-levels
;;         ;; :hook (after-init . global-flycheck-mode)
;;         :init (
;;                ;; flycheck-emacs-lisp-load-path 'inherit
;;                ;; flycheck-indication-mode (if (display-graphic-p)
;;                ;;                              'right-fringe
;;                ;;                            'right-margin)
;;                ;; ;; Only check while saving and opening files
;;                ;; flycheck-check-syntax-automatically '(save mode-enabled)
;;                setq flycheck-global-modes
;;                '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
;;                      org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)))
;;       (use-package dash :ensure t)
;;       (message "** ✅(OK): Successfully installed: s, flycheck and dash")
;;       t)
;;   (message "** ❌(FAIL): Failed to install s and dash"))

;; (defun enumerate-deep-local-folders (directory)
;;   "Recursively search for 'local' directories within a given DIRECTORY and enumerate their contents."
;;   (interactive)
;;   (dolist (entry (directory-files directory t))
;;     (unless (member (file-name-nondirectory entry) '("." ".."))
;;       (if (and (file-directory-p entry) (string-equal "local" (file-name-nondirectory entry)))
;;           (dolist (subdir (directory-files entry t))
;;             (unless (or (not (file-directory-p subdir))
;;                         (member (file-name-nondirectory subdir) '("." "..")))
;;               (message "%s" subdir))),
;;         (when (file-directory-p entry)
;;           (enumerate-deep-local-folders entry))))))

;; (defun enumerate-deep-local-folders (directory &optional accumulator)
;;   "Recursively search for 'local' directories within DIRECTORY and return their paths."
;;   (let ((entries (directory-files directory t)))
;;     (dolist (entry entries accumulator)
;;       (unless (member (file-name-nondirectory entry) '("." ".."))
;;         (if (and (file-directory-p entry) (string-equal "local" (file-name-nondirectory entry)))
;;             (push entry accumulator)
;;           (when (file-directory-p entry)
;;             (setq accumulator (enumerate-deep-local-folders entry accumulator))))))))


;; (defun invoke-create-package-file-for-locals (package-name)
;;   "Invoke `create-package-file` for each 'local' directory found by `enumerate-all-local-folders` with PACKAGE-NAME."
;;   (interactive "sPackage name: ")
;;   (let ((local-dirs (enumerate-all-local-folders)))
;;     (if (or (not local-dirs) (null local-dirs))
;;         (message "No 'local' directories found.")
;;       (dolist (dir local-dirs)
;;         (when (and dir (not (string-empty-p dir)))
;;           (create-package-file dir package-name))))))


;; (defun mahmoudimus/use-straight-for-evil-evilified-state (&rest args)
;;   "Function to run additional actions before `spacemacs-bootstrap/init-evil-evilified-state`."

;;   ;; if you pass a function as the arguement of :local-repo, it 
;;   ;; will not be evaluated.
;;   ;;
;;   ;; so we use quasiquote to achieve the same
;;   (eval 
;;     `(use-package evil-evilified-state
;;         :straight (
;;           :local-repo ,(concat user-emacs-directory "layers/+distributions/spacemacs-bootstrap/local/evil-evilified-state")))))
          
;; (defun mahmoudimus/use-straight-for-holy-mode(&rest args)          
;;   (eval 
;;     `(use-package holy-mode
;;         :straight (
;;           :local-repo ,(concat user-emacs-directory "layers/+distributions/spacemacs-bootstrap/local/holy-mode"))
;;         :no-require t
;;           )))
          
;; (defun mahmoudimus/use-straight-for-hybrid-mode(&rest args)          
;;   (eval 
;;     `(use-package hybrid-mode
;;         :straight (
;;           :local-repo ,(concat user-emacs-directory "layers/+distributions/spacemacs-bootstrap/local/hybrid-mode")))))

;; (defun mahmoudimus/use-straight-for-builtins(&rest args)
;;     (use-package dired-x 
;;        :straight (:type built-in)))
   
;; (defun mahmoudimus/use-straight-for-evil-unimpaired(&rest args)          
;;     (use-package evil-unimpaired
;;         :straight `(:local-repo ,(concat user-emacs-directory "layers/+spacemacs/spacemacs-evil/local/evil-unimpaired"))
;;         ))
  
;; ; (defun mahmoudimus/enable-straight(&rest args)    
;; ;   (message "enabling straight-use-package-by-default")    
;; ;   (setq straight-use-package-by-default t))


;; (advice-add 'spacemacs-bootstrap/init-evil-evilified-state :before #'mahmoudimus/use-straight-for-evil-evilified-state)
;; (advice-add 'spacemacs-bootstrap/init-holy-mode :before #'mahmoudimus/use-straight-for-holy-mode)
;; (advice-add 'spacemacs-bootstrap/init-hybrid-mode :before #'mahmoudimus/use-straight-for-hybrid-mode)
;; (advice-add 'spacemacs-defaults/init-dired-x :before #'mahmoudimus/use-straight-for-builtins)
;; (advice-add 'spacemacs-evil/init-evil-unimpaired :before #'mahmoudimus/use-straight-for-evil-unimpaired)
;; ; (advice-add ' /init-dired-x :after #'mahmoudimus/enable-straight)

