;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; 02-presets.el
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

;; override the spacemacs-env-vars-file to have the hostname in it to avoid clashing
;; as I sync my spacemacs-env-vars-file..
(setq spacemacs-env-vars-file
      (concat (concat (or dotspacemacs-directory user-home-directory)
                      ".spacemacs.env.")
              mahmoudimus-current-hostname))
              
              
;; disable prelude-flyspell (it is very slow)
(setq prelude-flyspell nil)

;; flyspell is slow, let's make it fast by ignoring the sit-for
;; see: http://www.brool.com/index.php/speeding-up-flyspell-region
(defadvice flyspell-region (around fast-flyspell-region)
  (cl-flet ( (sit-for (x) t) )
    ad-do-it))
(ad-activate 'flyspell-region)

;; gls

;; (let ((gls "/usr/local/bin/gls"))
;;   (if (file-exists-p gls)
;;       (setq insert-directory-program gls)))

(when (executable-find "gls")
  ;; Use GNU ls as 'gls' from 'coreutils' if available.
  (setq insert-directory-program "gls"))

;; (when (file-exists-p "/usr/local/bin/gls")
;;   (setq insert-directory-program "/usr/local/bin/gls"))