;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; core-packages.el
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

;; (setq debug-on-error t)
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; (unless (require 'use-package nil t)
;;   (straight-use-package 'use-package))

(setq straight-use-package-by-default t)

(defconst mahmoudimus-recipes-directory (expand-file-name "straight/recipes" spacemacs-dir)
  "Emacs config recipes directory.")

(defun enumerate-deep-local-folders (directory &optional accumulator)
  "Recursively search for 'local' directories within DIRECTORY, append their subdirectories to their paths, and return the modified paths."
  (let ((entries (directory-files directory t)))
    (dolist (entry entries accumulator)
      (unless (member (file-name-nondirectory entry) '("." ".."))
        (if (and (file-directory-p entry) (string-equal "local" (file-name-nondirectory entry)))
            (let ((subdirs (directory-files entry t)))
              (dolist (subdir subdirs)
                (unless (member (file-name-nondirectory subdir) '("." ".."))
                  (if (file-directory-p subdir)
                      (push (concat entry "/" (file-name-nondirectory subdir)) accumulator)))))
          (when (file-directory-p entry)
            (setq accumulator (enumerate-deep-local-folders entry accumulator))))))))

(defun enumerate-all-local-folders ()
  "Wrapper function to start enumeration from the root directory."
  (setq mahmoudimus--user-emacs-dir-absolute (expand-file-name user-emacs-directory))
  (enumerate-deep-local-folders (concat mahmoudimus--user-emacs-dir-absolute "layers/")))

(defun create-package-file (directory package-name)
  "Create a file named PACKAGE-NAME in `mahmmoudimus-recipes-directory` with a straight
   local-repo entry to DIRECTORY if it does not already exist.
   Write '($packagename :local-repo \"$path\")' into the file, where
   $packagename is replaced by PACKAGE-NAME and $path is the full path to the DIRECTORY."
  (interactive "DDirectory: \nsPackage name: ")
  (let ((full-path (expand-file-name package-name mahmoudimus-recipes-directory)))
    (if (file-exists-p full-path)
        (message "File already exists: %s" full-path)
      (with-temp-file full-path
        (insert (format "(%s :type nil :local-repo \"%s\")" package-name directory)))
      (message "File created: %s" full-path))))

(defun create-package-files-in-derived-locals ()
  "Create package files in each local directory enumerated by `enumerate-all-local-folders`,
   using the base name of the local directory as the package name."
  (interactive)
  (let ((local-dirs (enumerate-all-local-folders)))
    (if (null local-dirs)
        (message "No 'local' directories found.")
      (dolist (dir local-dirs)
        ;; Extract the package name from the directory path
        (let* ((package-name (file-name-nondirectory (directory-file-name dir)))
               (full-path (concat dir "/" package-name)))
          ;; Ensure the directory is valid and the file doesn't already exist.
          (when (and dir (not (string-empty-p dir)) (not (file-exists-p full-path)))
            (create-package-file dir package-name)))))))


(create-package-files-in-derived-locals)


(defun straight-recipes-mahmoudimus-retrieve (package)
  "Look up a PACKAGE recipe in mahmoudimus-recipes."
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally
           (expand-file-name (symbol-name package) mahmoudimus-recipes-directory))
          (read (current-buffer)))
      (error nil))))

(defun straight-recipes-mahmoudimus-list ()
  "Return a list of recipes available in MAHMOUDIMUS, as a list of strings."
  (straight--directory-files mahmoudimus-recipes-directory "^[^.]"))

(defun straight-recipes-mahmoudimus-version ()
  "Return the current version of the MAHMOUDIMUS retriever."
  1)

(straight-use-recipes '(mahmoudimus :type built-in :build nil))

;; make mahmoudimus has higher priority
(setq straight-recipe-repositories
      '(org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror mahmoudimus))

;;; Code:
(setq use-package-always-ensure nil)

;; Load essential ELisp libraries like `s-mode', `dash' and `f-mode'.
(use-package dash)    ;; “A modern list library for Emacs”
(use-package s)    ;; “The long lost Emacs string manipulation library”.
(use-package f)    ;;
(use-package pkg-info)    ;;
(use-package epl)    ;;
(use-package browse-kill-ring)    ;;
(use-package smart-mode-line)    ;;
(use-package rich-minority)    ;;
(use-package electric-operator)    ;;
(use-package rg)    ;;
(use-package deadgrep)    ;;
(use-package exec-path-from-shell)  ;;
(use-package elisp-format)  ;;
(use-package pcre2el)  ;;
(use-package treemacs)  ;;
(use-package ein)  ;;
(use-package direnv)  ;;
(use-package mac-pseudo-daemon)  ;;
(use-package el-patch)  ;;
(use-package helm-ag)  ;;
(use-package helm-flx)  ;;
(use-package company-fuzzy)  ;;
(use-package python-environment)  ;;
(use-package jedi)  ;;
(use-package virtualenvwrapper)  ;;
(use-package importmagic)  ;;
;; (use-package pyvenv)  ;;
(use-package pyenv-mode
  :straight (pyenv-mode
    :type git
    :host github
    :repo "pythonic-emacs/pyenv-mode"
  ))



;; ;;; the melpa package is already added
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; ;;(add-to-list 'package-archives
;; ;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (package-initialize)

;; ;; install the emacs packages above if they're not installed
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (defvar mahmoud-required-packages
;;   (append
;;    ;; general
;;    '(
;;      use-package

;;    ;;   browse-kill-ring
;;    ;;   ;; yasnippet (i'm going to get this from el-get)
;;    ;;   ;; https://github.com/Bruce-Connor/smart-mode-line
;;    ;;   ;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
;;    ;;   smart-mode-line
;;    ;;   ;; https://github.com/Bruce-Connor/rich-minority
;;    ;;   rich-minority
;;    ;;   electric-operator
;;    ;;   ;; deadgrep (https://github.com/Wilfred/deadgrep)
;;    ;;   deadgrep
;;    ;;   ;; rg.el (https://github.com/dajva/rg.el)
;;    ;;   rg
;;    ;;   ;; exec-path-from-shell.el (https://github.com/purcell/exec-path-from-shell)
;;    ;;   exec-path-from-shell
;;    ;;   ;; elisp-format
;;    ;;   elisp-format
;;    ;;   ;; emacs regex
;;    ;;   pcre2el
;;    ;;   ;; neotree
;;    ;;   ;; treemacs: https://github.com/Alexander-Miller/treemacs
;;    ;;   treemacs
;;    ;;   ;; https://github.com/millejoh/emacs-ipython-notebook
;;    ;;   ein
;;    ;;   ;; direnv
;;    ;;   ;; https://github.com/wbolster/emacs-direnv
;;    ;;   direnv
;;    ;;   ;; use-package
;;    ;;   ;; https://github.com/jwiegley/use-package
;;    ;;   use-package
;;    ;;   ;; Emacs "pseudo-daemon" for Mac OS
;;    ;;   ;; https://github.com/DarwinAwardWinner/mac-pseudo-daemon.git
;;    ;;   mac-pseudo-daemon
;;    ;;   ;; el-patch
;;    ;;   el-patch
;;    ;;   )
;;    ;; '()
;;    ;; ;; helm
;;    ;; '(helm-rg)
;;    ;; ;; company-mode
;;    ;; '(company-fuzzy)
;;    ;; '()
;;    ;; ;; python
;;    ;; '(python-environment
;;    ;;   jedi
;;    ;;   ;; https://github.com/porterjamesj/virtualenvwrapper.el
;;    ;;   virtualenvwrapper
;;    ;;   ;; importmagic
;;    ;;   importmagic
;;    ;;   ;; pyvenv
;;    ;;   pyvenv
;;      )
;;    )
;;   "A list of packages to ensure are installed at launch.")

;; (dolist (p mahmoud-required-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))
;;

     ;; ;; (f :location (recipe :fetcher github :repo "rejeep/f.el"))
     ;; ;; https://discourse.doomemacs.org/t/doom-cli-fails-with-file-missing-cannot-open-load-file-no-such-file-or-directory-pkg-info/3149/12
     ;; ;; https://discourse.doomemacs.org/t/doom-cli-fails-with-file-missing-cannot-open-load-file-no-such-file-or-directory-pkg-info/3149/13
     ;; f
     ;; dash
     ;; pkg-info
     ;; epl
     ;; ;; use-package
     ;; ;; https://github.com/jwiegley/use-package
     ;; use-package
     ;; browse-kill-ring
     ;; ;; https://github.com/Bruce-Connor/smart-mode-line
     ;; ;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
     ;; smart-mode-line
     ;; ;; https://github.com/Bruce-Connor/rich-minority
     ;; rich-minority
     ;; electric-operator
     ;; ;; deadgrep (https://github.com/Wilfred/deadgrep)
     ;; deadgrep
     ;; ;; rg.el (https://github.com/dajva/rg.el)
     ;; rg
     ;; ;; exec-path-from-shell.el (https://github.com/purcell/exec-path-from-shell)
;; (require 'use-package)
;; (setq use-package-always-ensure t)

(provide 'core-packages)


;;; core-packages.el ends here
