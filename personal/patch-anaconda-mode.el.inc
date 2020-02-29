;;; -*- mode: lisp -*-
;;; patch-anaconda-mode.el
;;
;; Copyright (c) 2011 -- Mahmoud Abdelkader
;;
;; Author: Mahmoud Abdelkader <mahmoud@linux.com>
;; URL: http://github.com/mahmoudimus/emacs.d/
;; Version: 1.0.0
;; Keywords: convenience, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file patches some stuff with anaconda mode

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

(require 'el-patch)

(setq mahmoudimus/patched-container-name "app")

(el-patch-feature anaconda-mode)
(with-eval-after-load 'anaconda-mode
  (el-patch-defun anaconda-mode-bootstrap-filter (process output &optional callback)
    "Set `anaconda-mode-port' from PROCESS OUTPUT.
Connect to the `anaconda-mode' server.  CALLBACK function will be
called when `anaconda-mode-port' will be bound."
  ;; Mimic default filter.
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point)))))
  (--when-let (s-match "anaconda_mode port \\([0-9]+\\)" output)
    (setq anaconda-mode-port (string-to-number (cadr it)))
    (set-process-filter process nil)
    (cond ((pythonic-remote-docker-p)
           (let* ((container-raw-description (with-output-to-string
                                               (with-current-buffer
                                                   standard-output
                                                 (call-process "docker" nil t nil "inspect" (pythonic-remote-host)))))
                  (container-description (let ((json-array-type 'list))
                                           (json-read-from-string container-raw-description)))
                  (container-ip (cdr (assoc 'IPAddress
                                            (cdadr (assoc 'Networks
                                                          (cdr (assoc 'NetworkSettings
                                                                      (car container-description)))))))))
             (setq anaconda-mode-socat-process
                   (start-process anaconda-mode-socat-process-name
                                  anaconda-mode-socat-process-buffer
                                  "socat"
                                  (format
                                   (el-patch-swap
                                     "TCP4-LISTEN:%d"
                                     "TCP4-LISTEN:%d,reuseaddr,fork")
                                   anaconda-mode-port)
                                  (format
                                   (el-patch-swap
                                     "TCP4:%s:%d"
                                     "EXEC:docker-compose exec -T %s 'socat STDIO TCP-CONNECT:localhost:%d'")
                                   (el-patch-swap container-ip mahmoudimus/patched-container-name)
                                   anaconda-mode-port)))
             (set-process-query-on-exit-flag anaconda-mode-socat-process nil)))
          ((pythonic-remote-vagrant-p)
           (setq anaconda-mode-ssh-process
                 (start-process anaconda-mode-ssh-process-name
                                anaconda-mode-ssh-process-buffer
                                "ssh" "-nNT"
                                (format "%s@%s" (pythonic-remote-user) (pythonic-remote-host))
                                "-p" (number-to-string (pythonic-remote-port))
                                "-L" (format "%s:%s:%s" anaconda-mode-port (pythonic-remote-host) anaconda-mode-port)))
           (set-process-query-on-exit-flag anaconda-mode-ssh-process nil)))
    (when callback
      (funcall callback)))))

(provide 'patch-anaconda-mode)
