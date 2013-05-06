;;; package --- Summary

;;; Commentary:

;;; Code:

(defun mahmoud-load-customization (target)
  "Load a file, TARGET, in the current directory by absolute pathifying it."
  (load-file (concat prelude-personal-dir "/" target)))

(mahmoud-load-customization "core-packages.el.inc")
(mahmoud-load-customization "core-el-get.el.inc")
(mahmoud-load-customization "core-editor.el.inc")
(mahmoud-load-customization "core-ui.el.inc")
(mahmoud-load-customization "core-programming.el.inc")

(mahmoud-load-customization "core-keybindings.el.inc")

;; specialize the programming languages
(mahmoud-load-customization "core-python.el.inc")


;;; mahmoud-customizations.el ends here
