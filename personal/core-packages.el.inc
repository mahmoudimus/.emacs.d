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

;;; Code:

;;; the melpa package is already added
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; install the emacs packages above if they're not installed
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar mahmoud-required-packages
  (append
   ;; general
   '(
     use-package

   ;;   browse-kill-ring
   ;;   ;; yasnippet (i'm going to get this from el-get)
   ;;   ;; https://github.com/Bruce-Connor/smart-mode-line
   ;;   ;; http://www.masteringemacs.org/articles/2012/09/10/hiding-replacing-modeline-strings/
   ;;   smart-mode-line
   ;;   ;; https://github.com/Bruce-Connor/rich-minority
   ;;   rich-minority
   ;;   electric-operator
   ;;   ;; deadgrep (https://github.com/Wilfred/deadgrep)
   ;;   deadgrep
   ;;   ;; rg.el (https://github.com/dajva/rg.el)
   ;;   rg
   ;;   ;; exec-path-from-shell.el (https://github.com/purcell/exec-path-from-shell)
   ;;   exec-path-from-shell
   ;;   ;; elisp-format
   ;;   elisp-format
   ;;   ;; emacs regex
   ;;   pcre2el
   ;;   ;; neotree
   ;;   ;; treemacs: https://github.com/Alexander-Miller/treemacs
   ;;   treemacs
   ;;   ;; https://github.com/millejoh/emacs-ipython-notebook
   ;;   ein
   ;;   ;; direnv
   ;;   ;; https://github.com/wbolster/emacs-direnv
   ;;   direnv
   ;;   ;; use-package
   ;;   ;; https://github.com/jwiegley/use-package
   ;;   use-package
   ;;   ;; Emacs "pseudo-daemon" for Mac OS
   ;;   ;; https://github.com/DarwinAwardWinner/mac-pseudo-daemon.git
   ;;   mac-pseudo-daemon
   ;;   ;; el-patch
   ;;   el-patch
   ;;   )
   ;; '()
   ;; ;; helm
   ;; '(helm-rg)
   ;; ;; company-mode
   ;; '(company-fuzzy)
   ;; '()
   ;; ;; python
   ;; '(python-environment
   ;;   jedi
   ;;   ;; https://github.com/porterjamesj/virtualenvwrapper.el
   ;;   virtualenvwrapper
   ;;   ;; importmagic
   ;;   importmagic
   ;;   ;; pyvenv
   ;;   pyvenv
     )
   )
  "A list of packages to ensure are installed at launch.")



(dolist (p mahmoud-required-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)
(setq use-package-always-ensure t)

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
(use-package pyvenv)  ;;

(setq use-package-always-ensure nil)

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

(provide 'core-packages)


;;; core-packages.el ends here
