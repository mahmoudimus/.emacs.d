;;; -*- mode: lisp -*-
;;; core-modeline.el
;;
;; Copyright (c) 2014 -- Mahmoud Abdelkader
;;
;; Author: Mahmoud Abdelkader <mahmoud@linux.com>
;; URL: http://github.com/mahmoudimus/emacs.d/
;; Version: 1.0.0
;; Keywords: convenience, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file allows me to control the modeline in my never ending
;; life mission to keep my emacs tidy

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
(require 'smart-mode-line)
(require 'rich-minority)

(sml/setup)
;; (sml/apply-theme 'dark)
;; (sml/apply-theme 'light)
;; (sml/apply-theme 'respectful)
;; (sml/apply-theme 'automatic)

;; Added in the right order, they even work sequentially:
(add-to-list 'sml/replacer-regexp-list '("^~/code/" ":CODE:") t)

;; rich-minority
;; rich-minority variables
;;
;; List of minor mode names that will be hidden from the minor-modes
;; list. Use this to hide only a few modes that are always active and
;; don’t really contribute information.
;;
(setq rm-blacklist '(" guru"))
;; List of minor mode names that are allowed on the minor-modes
;; list. Use this to hide all but a few modes.
;;
;; (add-to-list 'rm-whitelist '())
;; List text properties to apply to each minor-mode lighter. For
;; instance, by default we highlight Ovwrt with a red face, so you
;; always know if you’re in overwrite-mode.
;;
;; (add-to-list 'rm-text-properties '())


;; (defvar mode-line-cleaner-alist
;;   '((auto-complete-mode . " α")
;;     (yas-mode . " y")
;;     (paredit-mode . " π")
;;     (eldoc-mode . "")
;;     (abbrev-mode . "")
;;     (company-mode . " Ξ")
;;     (smartparens-mode . " ()")
;;     (ropemacs-mode . " ρ")
;;     (anaconda-mode . " ζ")
;;     (subword-mode . " Σ")
;;     ;; Major modes
;;     (lisp-interaction-mode . " λ")
;;     (hi-lock-mode . "")
;;     (python-mode . " Py")
;;     (emacs-lisp-mode . " ε")
;;     (nxhtml-mode . " nx")
;;     (helm-mode . " Η")
;;     )
;;   "Alist for `clean-mode-line'.

;; When you add a new element to the alist, keep in mind that you
;; must pass the correct minor/major mode symbol and a string you
;; want to use in the modeline *in lieu of* the original.
;; ;;; Greek letters - C-u C-\ greek ;; C-\ to revert to default
;; ;;; ς ε ρ τ υ θ ι ο π α σ δ φ γ η ξ κ λ ζ χ ψ ω β ν μ
;; ;;; http://en.wikipedia.org/wiki/Greek_alphabet
;; ")


(defun clean-mode-line ()
  "Replace modeline lighters with new ones."
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let ((mode (car cleaner))
                 (mode-str (cdr cleaner)))
             (when (assq mode minor-mode-alist)
                 (setcar (cdr (assq mode minor-mode-alist)) mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

;; (add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Φ"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

(provide 'core-modeline)
