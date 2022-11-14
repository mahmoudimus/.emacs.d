;;; package --- Summary

;;; Commentary:

;;; Code:

;; from https://emacs.stackexchange.com/a/33731/2082
(defun mahmoud-get-hostname ()
  "Reliable way to get current hostname.
`(getenv \"HOSTNAME\")' won't work because $HOSTNAME is NOT an
 environment variable.
`system-name' won't work because /etc/hosts could be modified"
  (with-temp-buffer
    (shell-command "hostname" t)
    (goto-char (point-max))
    (delete-char -1)
    (buffer-string)))

(defun mahmoud-load-customization (target)
  "Load a file, TARGET, in the current directory by absolute pathifying it."
  (message "Loading mahmoud customization: %s..." target)
  (load-file (concat spacemacs-personal-dir "/" target)))

;; packages
(mahmoud-load-customization "core-packages.el.inc")
(mahmoud-load-customization "core-el-get.el.inc")
;;(mahmoud-load-customization "config-path.el.inc")
;;(mahmoud-load-customization "use-config.el.inc")
(mahmoud-load-customization "core-lsp.el.inc")

;; editor & ui
(mahmoud-load-customization "core-editor.el.inc")
(mahmoud-load-customization "mac-pseudo-daemon.el.inc")
(mahmoud-load-customization "core-ui.el.inc")
(mahmoud-load-customization "core-modeline.el.inc")
(mahmoud-load-customization "core-programming.el.inc")

;; keybindings
(mahmoud-load-customization "core-keybindings.el.inc")

;; patches
(mahmoud-load-customization "patch-anaconda-mode.el.inc")

;; specialize the programming languages
;; (mahmoud-load-customization "core-python.el.inc")
(mahmoud-load-customization "core-golang.el.inc")
;;(mahmoud-load-customization "core-ruby.el.inc")
(mahmoud-load-customization "core-js.el.inc")

(mahmoud-load-customization "core-connectivity.el.inc")

;; https://emacs.stackexchange.com/questions/33728/how-do-i-retrieve-the-machines-hostname
;; load the customization based on the machine name that i am joining

(setq mahmoud-local-machine-name
      (concat "z-20-"
              (car (split-string (system-name) "\\."))
              "-machine.el.inc"))
(mahmoud-load-customization mahmoud-local-machine-name)

;;; mahmoud-customizations.el ends here

;; Customizations which we only want to happen when we're using
;; Emacs on a TTY.
;; (define-key function-key-map (kbd "ESC [ }") (kbd "<menu>")) ;%
;; (define-key function-key-map (kbd "ESC [ J") 'event-apply-super-modifier)
;; (define-key function-key-map (kbd "ESC [ ~") 'event-apply-hyper-modifier)
