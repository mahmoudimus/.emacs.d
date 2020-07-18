;;; -*- mode: emacs-lisp -*-
;;; core-el-get.el
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

(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; todo: configure this for non-spacemacs
;; (add-to-list 'el-get-recipe-path "~/.spacemacs.d/el-get-user/recipes")

;; https://github.com/dimitri/el-get#advanced-usage-with-local-recipes
;; https://github.com/atsutahiroshi/dotfiles/blob/aabad4dd4bc60a0237aa99d8e0a6f683612e4dc3/emacs/init-el-get.el
(setq el-get-sources
      '(escreen))

;; el-patch
(el-get-bundle el-patch
  :type git
  :url "https://github.com/raxod502/el-patch.git")

;; ;; python-mode
;; (el-get-bundle python-mode
;;   :type git
;;   :url "https://gitlab.com/python-mode-devs/python-mode.git")

;; (:name smex                          ;; a better (ido like) M-x
;;        :after (lambda ()
;;                 (setq smex-save-file "~/.emacs.d/.smex-items")
;;                 (global-set-key (kbd "M-x") 'smex)
;;                 (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;                 ))
;; (:name goto-last-change              ;; move pointer back to last change
;;        :after (lambda ()
;;                 (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

(setq required-packages
      (append
       '()
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync required-packages)

(provide 'core-el-get)
