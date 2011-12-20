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

;; load the python directory files
;; and set the variable
(setq python-files-dir (concat core-vendor-dir "python/"))
(add-to-list 'load-path python-files-dir)
(add-to-list 'load-path (concat core-vendor-dir "auto-complete.el"))
;; We never want to edit python bytecode
(add-to-list 'completion-ignored-extensions ".pyc")

;; enable the python mode
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode." t)

;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(setq pymacs-load-path (append (list (concat python-files-dir "rope/")
                                     (concat python-files-dir "ropemode/")
                                     (concat python-files-dir "pymacs/")
                                     (concat python-files-dir "ropemacs/"))
                                nil))
;;enable pymacs
(require 'python-mode)
(require 'auto-complete)
(require 'auto-complete-config)

;; python indentation hooks
;; TODO: clean this up a bit by hooking into a dispatcher function
(add-hook 'python-mode-hook
      (lambda ()
        (set-variable 'py-indent-offset 4)
        (set-variable 'py-continuation-offset 0)
        ;(set-variable 'py-smart-indentation nil)
        (set-variable 'indent-tabs-mode nil)
        (define-key py-mode-map (kbd "C-RET") 'newline-and-indent)
        (local-set-key (kbd "<M-S-iso-lefttab>") 'mahmoud-force-indent)
        (auto-complete-mode t)
      ))

(defun mahmoud-force-indent (&optional arg)
    (interactive "P")
    (insert-tab arg))


(defun reload-pymacs ()
    (interactive)
    (pymacs-terminate-services)
    (setenv "PYMACS_PYTHON"  (concat virtualenv-root "/" virtualenv-workon "/bin/python"))
    (pymacs-load "ropemacs" "rope-")
    (setq ropemacs-enable-autoimport 't))


;; load virtualenv
(add-to-list 'load-path (concat core-vendor-dir "virtualenv.el"))
(require 'virtualenv)

;; load flymake lint runner
(when (load "flymake" t)
  (defun flymake-python-lint-init (&optional trigger-type)
    ;; (print virtualenv-workon (get-buffer "*Messages*"))
    ;; (print virtualenv-root (get-buffer "*Messages*"))
    ;; (print buffer-file-name (get-buffer "*Messages*"))
    ;; (print file-local-variables-alist (get-buffer "*Messages*"))
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name)))
           (venv-path (concat virtualenv-root "/" virtualenv-workon))
           (options (when trigger-type (list "--trigger-type" trigger-type))))
      (if (and virtualenv-workon (not (string= virtualenv-workon " Virtualenv")))
          (if (listp options)
              (progn (push (concat "--virtualenv=" venv-path) options))
            (let (options (list (concat "--virtualenv=" venv-path))))))
      (list (concat (getenv "HOME") "/bin/flymake-python/pyflymake.py") (append options (list local-file)))))

  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-lint-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.wsgi\\'" flymake-python-lint-init)))

;; (defadvice flymake-python-lint-init (around flymake-python-lint-init-around)
;;   "Sets the virtual environment if applicable :)"
;;   (progn (print virtualenv-mode-name (get-buffer "*Messages*"))
;;          (print buffer-file-name (get-buffer "*Messages*"))
;;          (print file-local-variables-alist (get-buffer "*Messages*"))
;;          ;;(print (buffer-local-variables) (get-buffer "*Messages*"))
;;          ad-do-it))

;; add python hook when in python to activate flymake lint
(add-hook 'python-mode-hook (lambda() (flymake-mode t)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(provide 'core-python)
