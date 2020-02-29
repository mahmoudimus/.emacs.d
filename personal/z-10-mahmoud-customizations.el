;;; package --- Summary

;;; Commentary:

;;; Code:
(defun mahmoud-load-customization (target)
  "Load a file, TARGET, in the current directory by absolute pathifying it."
  (load-file (concat spacemacs-personal-dir "/" target)))

(mahmoud-load-customization "core-packages.el.inc")
(mahmoud-load-customization "core-el-get.el.inc")
(mahmoud-load-customization "core-editor.el.inc")
(mahmoud-load-customization "mac-pseudo-daemon.el.inc")
(mahmoud-load-customization "core-ui.el.inc")
(mahmoud-load-customization "core-modeline.el.inc")
(mahmoud-load-customization "core-programming.el.inc")

(mahmoud-load-customization "core-keybindings.el.inc")
(mahmoud-load-customization "patch-anaconda-mode.el.inc")

;; specialize the programming languages
;;(mahmoud-load-customization "core-python.el.inc")
;;(mahmoud-load-customization "core-ruby.el.inc")
(mahmoud-load-customization "core-js.el.inc")

(mahmoud-load-customization "core-connectivity.el.inc")



;;; mahmoud-customizations.el ends here

;; Customizations which we only want to happen when we're using
;; Emacs on a TTY.
;; (define-key function-key-map (kbd "ESC [ }") (kbd "<menu>")) ;%
;; (define-key function-key-map (kbd "ESC [ J") 'event-apply-super-modifier)
;; (define-key function-key-map (kbd "ESC [ ~") 'event-apply-hyper-modifier)
