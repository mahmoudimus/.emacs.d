;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; core-python.el
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

;; Enable elpy
;; (elpy-enable)

;; Use IPython for REPL
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt --no-banner --no-confirm-exit --colors=NoColor"
      python-shell-prompt-detect-failure-warning nil)


;; silence the warnings, while letting emacs still guess the offset
;; https://stackoverflow.com/a/51966682/133514
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; mode-alist should end .py or .wsgi
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.wsgi$" . python-mode) auto-mode-alist))
;; We never want to edit python bytecode
(add-to-list 'completion-ignored-extensions ".pyc")

;; setup virtualenvwrapper.el
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)
;; (setq-default mode-line-misc-info
;;               (cons '(:exec venv-current-name) mode-line-misc-info))
;; these vars in .dir-locals.el / file local variables are safe
;; (put 'project-venv-name 'safe-local-variable #'stringp)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


;; (defvar config-python-lsp-frontend 'lsp-mode)
;; (defvar config-python-lsp-backend 'ms-python)



;; this is an old version of my python customizations.

;; set the python file path and add a few things to the load path.
;;
;; (setq python-mode-dir (concat spacemacs-vendor-dir "/python-mode"))
;; (setq python-files-dir (concat spacemacs-vendor-dir "/python"))

;; I use python mode from launchpad.net (the one by python.org)
;; (require 'python-mode)
;; (autoload 'python-mode "python-mode" "Python editing mode." t)
;; python's python-mode.el comes with a few extensions for it,
;; in its /extensions and /completion folders.
;; not sure they're needed given helm/company mode
;; (add-to-list 'load-path (concat python-mode-dir "extensions"))
;; (add-to-list 'load-path (concat python-mode-dir "completion"))

;; (setq py-install-directory python-mode-dir)
;; (add-to-list 'load-path py-install-directory)
;; python mode from launchpad.net (the one by python.org)

;; creating and displaying an index menu of functions and global
;; variables is a huge performance problem.
;; (setq py-imenu-create-index-p nil)

;; use local default python with virtualenvwrapper
;; (setq py-use-local-default t)

;; (defun hook-py-venv-auto-workon ()
;;   "A python-mode hook, such that when the venv is activated:

;; - set the py-shell-local-path since we set the py-use-local-default
;; - once in a new venv, make sure we install some necessary tools.
;; "
;;   (hack-local-variables)
;;   (when (boundp 'project-venv-name)
;;     (venv-workon project-venv-name)
;;     (setq py-shell-local-path (concat venv-current-dir venv-executables-dir))
;;     ;; for anaconda mode
;;     (setq python-shell-virtualenv-path venv-current-dir)
;;     (setq virtualenv-name project-venv-name)
;;     ;; TODO: move this into a post activate hook and have it
;;     ;; TODO: do the upgrades as well from the .dir-locals.el
;;     (async-shell-command "pip install pylint flake8 nose pytest")
;;     ))
;; (add-hook 'python-mode-hook 'hook-py-venv-auto-workon)

;;;flycheck stuff
;; (when (not (boundp 'flycheck-mode))
;;   (flycheck-mode))

;; (setq flycheck-display-errors-delay 0.1)

(defun setup-py-flycheck ()
  ;; flake8
  (setq flycheck-python-flake8-executable (executable-find "flake8")
        flycheck-flake8rc (expand-file-name "~/.config/flake8"))
  ;; pylint
  (setq flycheck-python-pylint-executable (executable-find "pylint")
        flycheck-pylintrc (expand-file-name "~/.config/pylint"))
  ;; see https://github.com/flycheck/flycheck/issues/186, as the
  ;; author of flycheck believes that pylint and flake8 are not
  ;; complementary.
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  ;; (setq flycheck-checker 'python-pylint)

  ;; Force Flycheck mode on
  (flycheck-mode)
  )

(defun flycheck-python-setup ()
  "Just setups up local-variables to call flycheck to respect project
local settings."
  (add-hook 'hack-local-variables-hook #'setup-py-flycheck nil 'local))
;;(add-hook 'python-mode-hook #'flycheck-python-setup)


;; Setup the emacs' python-environment [#]_
;; Typically used for building python enhancements to emacs via python.
;; .. [#] https://github.com/tkf/emacs-python-environment"
;; (require 'python-environment)
;; (setq python-environment-directory venv-location)
;; (setq python-environment-default-root-name "default") ; symlink

;; (defun pymacs-make-env ()
;;   (interactive)
;;   (python-environment-run "pip" "install" "Pymacs"))


;; (defun setup-jedi ()
;;   "Setup jedi"
;;   ;; https://gist.github.com/nyergler/6100112
;;   (require 'jedi)
;;   (setq jedi:complete-on-dot t)
;;   (setq jedi:server-command '("jediepcserver")) ; on PATH in every venv

;;   (defun local-jedi-server-setup ()
;;     (hack-local-variables)
;;     (when (boundp 'project-venv-name)
;;       (venv-workon project-venv-name))
;;     (jedi:setup))

;;   (add-hook 'python-mode-hook 'local-jedi-server-setup)
;;   )


(defun setup-ipython ()
  "Setup ipython"
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
                "ipython")
  )

;; (defun core-setup-python-env ()
;;   (interactive)
;;   (setenv "PYTHONPATH"
;;           (concat
;;            (getenv "PYTHONPATH") path-separator
;;            python-files-dir
;;            )))

;; (defun reload-pymacs ()
;;     (interactive)
;;     (pymacs-terminate-services)
;;     (setenv "PYMACS_PYTHON"  (concat py-shell-local-path "python"))
;;     (pymacs-load "ropemacs" "rope-")
;;     (setq ropemacs-enable-autoimport 't))


;; (defun setup-ropemacs ()
;;   "Setup the ropemacs harness"
;;   (core-setup-python-env)
;;   (pymacs-load "ropemacs" "rope-")

;;   ;; Stops from erroring if there's a syntax err
;;   (setq ropemacs-codeassist-maxfixes 3)

;;   ;; Configurations
;;   (setq ropemacs-guess-project t)
;;   (setq ropemacs-enable-autoimport t)


;;   (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"))

;;   ;; Adding hook to automatically open a rope project if there is one
;;   ;; in the current or in the upper level directory
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (cond ((file-exists-p ".ropeproject")
;;                      (rope-open-project default-directory))
;;                     ((file-exists-p "../.ropeproject")
;;                      (rope-open-project (concat default-directory "..")))
;;                     )
;;               ))
;;   )

(defun mahmoud-force-indent (&optional arg)
    (interactive "P")
    (insert-tab arg))

;; pymacs
;; (eval-after-load 'pymacs
;;   '(progn
;;      ;;(message (shell-command-to-string "python -c 'import sys; print sys.path'"))
;;      (pymacs-make)
;;      ;; (message "Pymacs has been loaded, now adding to the path...")
;;      ;; (add-to-list 'pymacs-load-path python-files-dir)
;;      ))

;; (require 'pymacs)

;; https://github.com/douglasdavis/ddavis.io/blob/master/content/posts/emacs-python-lsp.md#automated-helper-automated-helper

;; (defun dd/py-workon-project-venv ()
;;   "Call pyenv-workon with the current projectile project name.
;; This will return the full path of the associated virtual
;; environment found in $WORKON_HOME, or nil if the environment does
;; not exist."
;;   (let ((pname (projectile-project-name)))
;;     (pyvenv-workon pname)
;;     (if (file-directory-p pyvenv-virtual-env)
;;         pyvenv-virtual-env
;;       (pyvenv-deactivate))))

;; (defun dd/py-auto-lsp ()
;;   "Turn on lsp mode in a Python project with some automated logic.
;; Try to automatically determine which pyenv virtual environment to
;; activate based on the project name, using
;; `dd/py-workon-project-venv'. If successful, call `lsp'. If we
;; cannot determine the virtualenv automatically, first call the
;; interactive `pyvenv-workon' function before `lsp'"
;;   (interactive)
;;   (let ((pvenv (dd/py-workon-project-venv)))
;;     (if pvenv
;;         (lsp)
;;       (progn
;;         (call-interactively #'pyvenv-workon)
;;         (lsp)))))

;; (bind-key (kbd "C-c C-a") #'dd/py-auto-lsp python-mode-map)

;; (add-hook 'python-mode-hook
;;       (lambda ()
;;         (set-variable 'py-indent-offset 4)
;;         (set-variable 'py-continuation-offset 0)
;;         ;(set-variable 'py-smart-indentation nil)
;;         (set-variable 'indent-tabs-mode nil)
;;         (define-key py-mode-map (kbd "C-RET") 'newline-and-indent)
;;         (local-set-key (kbd "<M-S-iso-lefttab>") 'mahmoud-force-indent)
;;         (auto-complete-mode t)
;;       ))

;;;delayed hooks after python-mode
(eval-after-load 'python-mode
  '(progn
     (setup-ipython)
     )
  )

(provide 'core-python)
