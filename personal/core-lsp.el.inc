;;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;; core-lsp.el.inc
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

;; don't ping LSP lanaguage server too frequently
(defvar lsp-on-touch-time 0)
(defadvice lsp-on-change (around lsp-on-change-hack activate)
  ;; don't run `lsp-on-change' too frequently
  (when (> (- (float-time (current-time))
              lsp-on-touch-time) 30) ;; 30 seconds
    (setq lsp-on-touch-time (float-time (current-time)))
    ad-do-it))

(defun mahmoudimus/lsp-connect (&optional no-reconnect)
  "Connect lsp server.  If NO-RECONNECT is t, don't shutdown existing lsp connection."
  (interactive "P")
  (when (and (not no-reconnect)
             (fboundp 'lsp-disconnect))
    (lsp-disconnect))
  (when (and buffer-file-name
             (not (member (file-name-extension buffer-file-name)
                          '("json"))))
    (unless (and (boundp 'lsp-mode) lsp-mode)
      (if (derived-mode-p 'js2-mode) (setq-local lsp-enable-imenu nil))
      (lsp-deferred))))

;; @see https://github.com/emacs-lsp/lsp-mode/pull/1498 and code related to auto configure.
;; Require clients could be slow.
;; I only load `lsp-clients' because it includes the js client which I'm interested
;; (setq lsp-client-packages '(lsp-clients))

;; don't scan 3rd party javascript libraries
;; (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored) ; json


(provide 'core-lsp)
