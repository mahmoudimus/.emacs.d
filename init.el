;;; init.el
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

;; set your custom file so you don't clutter up your init.el
(setq expanded-user-emacs-directory (expand-file-name user-emacs-directory))
(setq custom-file (concat expanded-user-emacs-directory "custom.el"))

(defvar core-modules-dir (concat expanded-user-emacs-directory "modules/")
  "This directory houses all of my modules. Modify at your own risk.")
(defvar core-vendor-dir (concat expanded-user-emacs-directory "vendor/")
  "This directory house Emacs Lisp packages that are not yet available in
ELPA (or Marmalade).")

(add-to-list 'load-path core-modules-dir)
(add-to-list 'load-path core-vendor-dir)

;; core modules
(require 'core-packages)
(require 'core-el-get)
(require 'core-ui)
(require 'core-editor)
(require 'core-keybindings)

;; language support
(require 'core-programming)
(require 'core-python)

;;
;; load the custom file
(load custom-file 'noerror)
