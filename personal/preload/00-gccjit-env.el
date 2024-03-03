;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; 00-gccjit-env.el
;

(when (eq system-type 'darwin)
  (if (>= emacs-major-version 28)
      ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/323
      ;; And sure enough the workaround broke with gcc going to version 12, new line:
      (setenv "LIBRARY_PATH" "/usr/local/Cellar/gcc/12.2.0/lib/gcc/12:/usr/local/Cellar/libgccjit/12.2.0/lib/gcc/12:/usr/local/Cellar/gcc/12.2.0/lib/gcc/12/gcc/x86_64-apple-darwin20/12")))
