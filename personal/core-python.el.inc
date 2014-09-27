;;; -*- mode: lisp -*-
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

;; set the python file path and add a few things to the load path.
(setq python-mode-dir (concat prelude-vendor-dir "/python-mode"))
(setq python-files-dir (concat prelude-vendor-dir "/python"))

(add-to-list 'load-path (concat python-files-dir "/virtualenv.el"))
(add-to-list 'load-path (concat python-mode-dir "extensions"))
(add-to-list 'load-path (concat python-mode-dir "completion"))

(setq py-install-directory python-mode-dir)
(add-to-list 'load-path py-install-directory)
;; python mode from launchpad.net (the one by python.org)

;; creating and displaying an index menu of functions and global
;; variables is a huge performance problem.
(setq py-imenu-create-index-p nil)

;; python mode settings
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
;; We never want to edit python bytecode
(add-to-list 'completion-ignored-extensions ".pyc")

(require 'auto-complete-config)
(ac-config-default)

;; disable smart operator cuz it fucking sucks
;; (setq py-smart-operator-mode-p nil)

(eval-after-load "pymacs"
  '(progn
     ;;(message (shell-command-to-string "python -c 'import sys; print sys.path'"))
     (message "Pymacs has been loaded, now adding to the path...")
     (add-to-list 'pymacs-load-path python-files-dir)
     ))


(defun setup-nose ()
  "Sets up nosetests"
  (require 'nose)
  )

(defun setup-ropemacs ()
  "Setup the ropemacs harness"
  (core-setup-python-env)
  (pymacs-load "ropemacs" "rope-")

  ;; Stops from erroring if there's a syntax err
  (setq ropemacs-codeassist-maxfixes 3)

  ;; Configurations
  (setq ropemacs-guess-project t)
  (setq ropemacs-enable-autoimport t)


  (setq ropemacs-autoimport-modules '("os" "shutil" "sys" "logging"))

  ;; Adding hook to automatically open a rope project if there is one
  ;; in the current or in the upper level directory
  (add-hook 'python-mode-hook
            (lambda ()
              (cond ((file-exists-p ".ropeproject")
                     (rope-open-project default-directory))
                    ((file-exists-p "../.ropeproject")
                     (rope-open-project (concat default-directory "..")))
                    )
              ))
  )

(defun mahmoud-force-indent (&optional arg)
    (interactive "P")
    (insert-tab arg))


(defun core-setup-python-env ()
  (interactive)
  (setenv "PYTHONPATH"
          (concat
           (getenv "PYTHONPATH") path-separator
           python-files-dir
           ))
)

(defun reload-pymacs ()
    (interactive)
    (pymacs-terminate-services)
    (setenv "PYMACS_PYTHON"  (concat virtualenv-root "/" virtualenv-workon "/bin/python"))
    (pymacs-load "ropemacs" "rope-")
    (setq ropemacs-enable-autoimport 't))



;; load flymake lint runner
;; (when (load "flymake" t)
;;   (defun flymake-python-lint-init (&optional trigger-type)
;;     ;; (print virtualenv-workon (get-buffer "*Messages*"))
;;     ;; (print virtualenv-root (get-buffer "*Messages*"))
;;     ;; (print buffer-file-name (get-buffer "*Messages*"))
;;     ;; (print file-local-variables-alist (get-buffer "*Messages*"))
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        (if (fboundp 'flymake-create-temp-copy)
;;                            'flymake-create-temp-copy
;;                          'flymake-create-temp-inplace)))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name)))
;;            (venv-path (concat virtualenv-root "/" virtualenv-workon))
;;            (options (when trigger-type (list "--trigger-type" trigger-type))))
;;       (if (and virtualenv-workon (not (string= virtualenv-workon " Virtualenv")))
;;           (if (listp options)
;;               (progn (push (concat "--virtualenv=" venv-path) options))
;;             (let (options (list (concat "--virtualenv=" venv-path))))))
;;       (list
;;        (concat (getenv "HOME") "/bin/flymake-python/pyflymake.py")
;;        (append options (list local-file)))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-python-lint-init))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.wsgi\\'" flymake-python-lint-init)))

;; (defadvice flymake-python-lint-init (around flymake-python-lint-init-around)
;;   "Sets the virtual environment if applicable :)"
;;   (progn (print virtualenv-mode-name (get-buffer "*Messages*"))
;;          (print buffer-file-name (get-buffer "*Messages*"))
;;          (print file-local-variables-alist (get-buffer "*Messages*"))
;;          ;;(print (buffer-local-variables) (get-buffer "*Messages*"))
;;          ad-do-it))



(defun setup-python-environment ()
  "Setup the emacs' python-environment [#]_

.. [#] https://github.com/tkf/emacs-python-environment"

  (require 'python-environment)
  (setq python-environment-directory "~/.virtualenvs")
  (setq python-environment-default-root-name "default") ; symlink
  (setup-virtualenvwrapper)
  )


(defun setup-virtualenvwrapper ()
  "Setup virtualenvwrapper"

  (require 'virtualenvwrapper)
  (setq venv-location (file-name-as-directory python-environment-directory))
  (put 'project-venv-name 'safe-local-variable #'stringp)
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  )



(defun setup-jedi ()
  "Setup jedi"
  ;; https://gist.github.com/nyergler/6100112
  (require 'jedi)
  (setq jedi:complete-on-dot t)
  (setq jedi:server-command '("jediepcserver")) ; on PATH in every venv

  (defun local-jedi-server-setup ()
    (hack-local-variables)
    (when (boundp 'project-venv-name)
      (venv-workon project-venv-name))
    (jedi:setup))

  (add-hook 'python-mode-hook 'local-jedi-server-setup)
  )


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
  )


(eval-after-load 'python-mode
  '(progn
     ;;==================================================
     ;; Ropemacs Configuration
     ;;==================================================
     (setup-ropemacs)
     ;; ;;==================================================
     ;; ;; Virtualenv Commands
     ;; ;;==================================================
     ;; ;; load virtualenv
     ;; (require 'virtualenv)
     ;;==================================================
     ;; Python Environment
     ;;==================================================
     (setup-python-environment)
     ;;==================================================
     ;; Jedi
     ;;==================================================
     ;; (setup-jedi)
     ;;==================================================
     ;; Pycomplete
     ;;==================================================
     ;; load pycomplete
     (require 'pycomplete)
     ;;==================================================
     ;; auto-pair
     ;;==================================================
     ;; load autopair
     ;; (require 'autopair)
     ;; (autopair-global-mode)
     ;; (setq autopair-autowrap t)
     ;;==================================================
     ;; Nose Tests
     ;;==================================================
     ;; (setup-nose)
     ;; add python hook when in python to activate flymake lint
     (add-hook 'python-mode-hook
               (lambda()
                 (flymake-mode t)
                 ;; Not on all modes, please
                 (flymake-find-file-hook)
                 )
               )
     )
  )
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



;; ;; pymacs
(setq py-load-pymacs-p t)

;; require python
(require 'python-mode)

(provide 'core-python)
