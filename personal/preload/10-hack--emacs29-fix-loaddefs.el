;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-

;;; Code:

;; for emacs29 there's some work that has to be done for an issue regarding
;; loaddefs-generate:
;; see: https://github.com/doomemacs/doomemacs/commit/7e931ec58634e6499d91c71c75d31f6a0e498e77
;; HACK: This fixes an issue present in recent builds of Emacs 29. See
;;   emacs-mirror/emacs@0d383b592c2f. Straight.el uses `loaddefs-generate' if it
;;   is available, which activates `emacs-lisp-mode' to read autoloads files,
;;   but does so without suppressing its hooks. Some packages (like overseer)
;;   add hooks to `emacs-lisp-mode-hook' in their autoloads, and once triggered,
;;   they will try to load their dependencies (like dash or pkg-info), causing
;;   file errors.
;; REVIEW: Report this upstream.
(defadvice! doom--fix-loaddefs-generate--parse-file-a (fn &rest args)
  :around #'loaddefs-generate--parse-file
  (let (emacs-lisp-mode-hook)
    (apply fn args)))
