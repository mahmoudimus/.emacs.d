;;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; core-connectivity.el
;;
;; Copyright (c) 2015 -- Mahmoud Abdelkader
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

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(defcustom docker-command "docker"
  "The command for \\[docker] package."
  :type 'string
  :group 'docker)

(defcustom docker-compose-command "docker-compose"
  "The command for \\[docker-compose] package."
  :type 'string
  :group 'docker)

;; ;;`tramp-container' is builtin since 29
;; ;; (eval-and-compile
;; ;;   (if (>= emacs-major-version 29)
;; ;;       (require 'tramp-container)
;; ;;     (require 'docker-tramp)))
;; (if (setq tmp--docker-path (executable-find "docker"))
;;     (if (>= emacs-major-version 29)
;;         (use-package tramp-container :ensure nil)
;;       (use-package docker-tramp
;;         :ensure t
;;         :config
;;         ;; docker-tramp -- only if docker is installed
;;         ;; takes forever... (exec-path-from-shell-copy-env "DOCKER_HOST")
;;         (setq docker-tramp-docker-executable tmp--docker-path))))

(defun docker-flow-select-container-name (name)
  (interactive (list (docker-flow-read-container-name "Select container: "))))

(defun docker-flow-read-container-name (prompt)
  "Read an container name using PROMPT."
  (completing-read prompt (-map #'car (docker-flow-container-names))))

(defun docker-flow-container-names ()
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((filter "[{{json .Names}}]")
         (data (docker "ps" (format "--format=\"%s\"" filter) "-a"))
         (lines (s-split "\n" data t)))
    (-map #'docker-flow-container-parse lines)))

(defun docker-flow-container-parse (line)
  "Convert a LINE from \"docker ps\" to a `tabulated-list-entries' entry."
  (let (data)
    (condition-case err
        (setq data (json-read-from-string line))
      (json-readtable-error
       (error "could not read following string as json:\n%s" line)))
    (list (aref data 0) data)))

(defun docker (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "%s %s %s" "docker" action (s-join " " (-non-nil args)))))
    (shell-command-to-string command)))

;; look into https://github.com/MacFarlaneBro/docker-compose.el

(defun docker-compose (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "%s %s %s" docker-compose-command action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))


(defun mahmoudimus/should-run-docker-compose ()
  "Determines if docker-compose should be used."
  (and (file-exists-p  "docker-compose.yml")
       (executable-find "docker")
       (executable-find "docker-compose")))


(provide 'core-connectivity)

;;; core-connectivity.el ends here
