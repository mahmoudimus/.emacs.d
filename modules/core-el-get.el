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

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq el-get-sources
      '((:name magit
               :after (lambda ()
                        (global-set-key (kbd "C-x g") 'magit-status)))
        (:name yasnippet
       :website "http://code.google.com/p/yasnippet/"
       :description "YASnippet is a template system for Emacs."
       :type git
       :url "https://github.com/capitaomorte/yasnippet.git"
       :features "yasnippet"
       :prepare (lambda ()
                  ;; Set up the default snippets directory
                  ;;
                  ;; Principle: don't override any user settings
                  ;; for yas/snippet-dirs, whether those were made
                  ;; with setq or customize.  If the user doesn't
                  ;; want the default snippets, she shouldn't get
                  ;; them!
                  (unless (or (boundp 'yas/snippet-dirs) (get 'yas/snippet-dirs 'customized-value))
                    (setq yas/snippet-dirs
                          (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets")))))
       :post-init (lambda ()
                    ;; Trick customize into believing the standard
                    ;; value includes the default snippets.
                    ;; yasnippet would probably do this itself,
                    ;; except that it doesn't include an
                    ;; installation procedure that sets up the
                    ;; snippets directory, and thus doesn't know
                    ;; where those snippets will be installed.  See
                    ;; http://code.google.com/p/yasnippet/issues/detail?id=179
                    (put 'yas/snippet-dirs 'standard-value
                         ;; as cus-edit.el specifies, "a cons-cell
                         ;; whose car evaluates to the standard
                         ;; value"
                         (list (list 'quote
                                     (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets"))))))
       ;; byte-compile load vc-svn and that fails
       ;; see https://github.com/dimitri/el-get/issues/200
       :compile nil)))

(setq required-packages
      (append
       '()
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync required-packages)

(provide 'core-el-get)
