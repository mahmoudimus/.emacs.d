;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; core-editor.el
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

(setq confirm-kill-emacs 'y-or-n-p)
;; slowness :-(
(setq gc-cons-threshold 100000000)

(setq projectile-enable-caching t)
;; I want my copies in the system temp dir.
;; Do not run flymake in place (for TRAMP support - see
;;    https://github.com/illusori/emacs-flymake/issues/1
;; )
;; (setq flymake-run-in-place nil)



(use-package electric-operator
  :delight
  :config
  ;; (add-hook 'python-mode-hook 'electric-operator-mode)
  ;; (apply #'electric-operator-add-rules-for-mode 'python-mode
  ;;   (electric-operator-add-rules-for-mode 'python-mode
  ;;                                         (cons "<-" " <- ")
  ;;                                         (cons "->" " -> ")
  ;;                                         (cons "=>" " => ")
  ;;                                         (cons "/*" " /* ")
  ;;                                         (cons "//" " // ")
  ;;                                         (cons "<<" " << ")
  ;;                                         ))
  )

;; ------------------------------------
;; require browse-kill-ring
;; ------------------------------------
(require 'browse-kill-ring)

;; company-mode
;;(global-company-fuzzy-mode 1)
;;(setq company-fuzzy-sorting-backend 'alphabetic)
;;
(setq company-fuzzy-show-annotation t)

;; helm

;; https://github.com/Havner/emacs-config/blob/0cb1a1f41d4c9b06b752cda0cbcbf8706cd8504b/config.org
(defun mahmoudimus/kill-buffers-helm ()
  (interactive)
  (let ((buffers (buffer-list)))
    (while buffers
      (let* ((buffer (car buffers))
             (bname (buffer-name buffer)))
        (if (and (string-prefix-p "*helm" bname t)
                 (not (string-equal bname helm-last-buffer)))
            (kill-buffer buffer)))
      (setq buffers (cdr buffers)))))

;; look more here:
;; -- https://github.com/PythonNut/quark-emacs/blob/5c181c465eb6046c1d30e7b9a8626da77abcfe2d/modules/config-helm.el
;; helm settings
(use-package helm
  :config

  (helm-flx-mode +1)
  ;; (helm-flx-historian-mode +1)

  (set-face-attribute 'helm-selection nil :underline nil)

  ;; (setq helm-always-two-windows t)
  ;; (setq helm-split-window-default-side 'right)
  ;; (setq helm-split-window-inside-p t)
  ;; (setq helm-display-buffer-default-height 0.3)

  ;; for the eshell-pcomplete and other using with-helm-show-completion
  ;; (setq helm-show-completion-display-function #'helm-show-completion-default-display-function)
  ;; (setq helm-show-completion-min-window-height 15)

  (setq helm-bookmark-show-location t)
  (setq helm-buffer-max-length 30)
  (setq helm-candidate-number-limit 100)
  ;; (setq helm-completion-style 'emacs)
  (setq helm-echo-input-in-header-line t)
  (setq helm-ff-candidate-number-limit 100)
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-findutils-search-full-path t)
  (setq helm-locate-fuzzy-match t)
  (if (eq system-type 'darwin)
      (setq helm-locate-command "/usr/local/bin/glocate %s -e -A --regex %s"
            helm-locate-recursive-dirs-command "/usr/local/bin/glocate -i -e -A --regex '^%s' '%s.*$'"))

  (setq helm-ag-insert-at-point 'symbol)

  (add-hook 'helm-quit-hook #'mahmoudimus/kill-buffers-helm)

  (setq helm-case-fold-search 'smart
        helm-candidate-separator (make-string 20 ?─)
        helm-inherit-input-method nil))

;; uncomment for emacs 26.3
;; (setq completion-styles '(helm-flex))
;; for emacs 27+
(setq completion-styles '(flex))

;; snippets
;;
;; https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bcompletion/auto-completion#snippets-directories
;; load all el files in the snippets directory, they're usually lisp
;; helpers that help with snippet expansions.
(defvar core-custom-snippets-dir (expand-file-name "snippets" dotspacemacs-directory)
  "This folder houses my custom yasnippet bundles distributed with my emacs.el.")

;; https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bcompletion/auto-completion#snippets-directories
(setq auto-completion-private-snippets-directory core-custom-snippets-dir)

;; use yasnippet
(require 'yasnippet)
;; term-mode does not play well with yasnippet
(add-hook 'term-mode-hook (lambda ()
                            (yas-minor-mode -1)))

;; Load the snippets
(yas-global-mode 1)
;; (yas/global-mode t)
;; (add-to-list 'yas-snippet-dirs core-custom-snippets-dir)
;; (mapc 'load (directory-files core-custom-snippets-dir t "^[^#].*el$"))
;; new version of yasnippet, loads directory
;; -- https://stackoverflow.com/questions/9698415/how-to-build-list-with-defvar-in-emacs
(setq yas-snippet-dirs (cons core-custom-snippets-dir yas-snippet-dirs))
(delq nil (delete-dups yas-snippet-dirs))

;; duplicate line
;; code borrowed from http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the
original" (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; Mark whole line
(defun mark-line (&optional arg)
  "Marks a line"
  (interactive "p")
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

; code copied from http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

; patches by balle
; http://www.datenterrorist.de
(defun balle-python-shift-left ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
       mark-active)
    (setq start (region-beginning) end (region-end))
      (progn
    (setq bds (bounds-of-thing-at-point 'line))
    (setq start (car bds) end (cdr bds))))
  (python-indent-shift-left start end))
  (setq deactivate-mark nil))

(defun balle-python-shift-right ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
       mark-active)
    (setq start (region-beginning) end (region-end))
      (progn
    (setq bds (bounds-of-thing-at-point 'line))
    (setq start (car bds) end (cdr bds))))
  (python-indent-shift-right start end))
  (setq deactivate-mark nil))

;; join paragraphs

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun mahmoudimus/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(defun mahmoudimus/toggle-fill-paragraph ()
  ;; Based on http://xahlee.org/emacs/modernization_fill-paragraph.html
  "Fill or unfill the current paragraph, depending upon the current line length.
When there is a text selection, act on the region.
See `fill-paragraph' and `fill-region'."
  (interactive)
  ;; We set a property 'currently-filled-p on this command's symbol
  ;; (i.e. on 'toggle-fill-paragraph), thus avoiding the need to
  ;; create a variable for remembering the current fill state.
  (save-excursion
    (let* ((deactivate-mark nil)
           (line-length (- (line-end-position) (line-beginning-position)))
           (currently-filled (if (eq last-command this-command)
                                 (get this-command 'currently-filled-p)
                               (< line-length fill-column)))
           (fill-column (if currently-filled
                            most-positive-fixnum
                          fill-column)))
      (if (region-active-p)
          (fill-region (region-beginning) (region-end))
        (fill-paragraph))
      (put this-command 'currently-filled-p (not currently-filled)))))

;;; Open the current file with vscode
(defun open-by-vscode ()
  (interactive)
  (shell-command (format "code-insiders -r -g %s:%d:%d" (buffer-file-name)
                         (line-number-at-pos)
                         (current-column))))

(define-key global-map (kbd "C-c C-v" )  'open-by-vscode)


(provide 'core-editor)

;;; core-editor.el ends here
