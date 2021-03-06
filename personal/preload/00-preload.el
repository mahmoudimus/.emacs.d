;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; preload-01.el
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
;; disable prelude-flyspell (it is very slow)
(setq prelude-flyspell nil)

;; flyspell is slow, let's make it fast by ignoring the sit-for
;; see: http://www.brool.com/index.php/speeding-up-flyspell-region
(defadvice flyspell-region (around fast-flyspell-region)
  (cl-flet ( (sit-for (x) t) )
    ad-do-it))
(ad-activate 'flyspell-region)

;; override the spacemacs-env-vars-file to have the hostname in it to avoid clashing
;; as I sync my spacemacs-env-vars-file..
(setq spacemacs-env-vars-file
      (concat (concat (or dotspacemacs-directory user-home-directory)
                      ".spacemacs.env.")
              (system-name)))

;; Use `fundamental-mode' to reduce startup time
(setq initial-major-mode 'lisp-interaction-mode)

;; `lexical-binding' is on in *scratch* since 27.1
(when (version< emacs-version "27.1")
  (add-hook 'emacs-startup-hook
            (defun mahmoudimus-enable-lexical-binding ()
              (let ((buffer (get-buffer "*scratch*")))
                (when buffer
                  (with-current-buffer buffer
                    (setq lexical-binding t)))))))


;; (let ((gls "/usr/local/bin/gls"))
;;   (if (file-exists-p gls)
;;       (setq insert-directory-program gls)))

(when (executable-find "gls")
  ;; Use GNU ls as 'gls' from 'coreutils' if available.
  (setq insert-directory-program "gls"))

;; (when (file-exists-p "/usr/local/bin/gls")
;;   (setq insert-directory-program "/usr/local/bin/gls"))

(provide 'preload-01)

;;; preload-01.el ends here
