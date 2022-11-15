;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; 01-constants.el
;;
;; Copyright (c) 2014 -- Mahmoud Abdelkader
;;
;; Author: Mahmoud Abdelkader <mahmoud@linux.com>
;; URL: http://github.com/mahmoudimus/emacs.d/
;; Version: 1.0.0
;; Keywords: convenience, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the preloads for prelude to make
;; the loading process for emacs much faster
;;; Code:

;; Use `fundamental-mode' to reduce startup time
(setq initial-major-mode 'lisp-interaction-mode)

;; from https://emacs.stackexchange.com/a/33731/2082
(defun mahmoudimus/get-hostname ()
  "Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified"
  (with-temp-buffer
    (shell-command "hostname" t)
    (goto-char (point-max))
    (delete-char -1)
    (buffer-string)))

(setq mahmoudimus-current-hostname 
  (car (split-string (system-name) "\\.")))
