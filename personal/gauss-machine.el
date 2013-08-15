;;; package --- Summary
;; This is my MacBook Pro 17"

;;; Commentary:

;;; Code:

;; Make command Super
(setq ns-command-modifier (quote super))
;; Make option Meta
(setq ns-alternate-modifier (quote meta))
;; Make the function key Alt
(setq ns-function-modifier (quote alt))

;; Make alt be meta instead of command, so we can map command stuff to
;; normal cut and paste.
;; (setq mac-command-modifier 'alt
;;       mac-option-modifier 'meta)

;; Map command-x,c,v to cut, copy, paste
;; (global-set-key [?\A-x] 'clipboard-kill-region)
;; (global-set-key [?\A-c] 'clipboard-kill-ring-save)
;; (global-set-key [?\A-v] 'clipboard-yank)

;; More mac shortcuts.
;; (global-set-key [?\A-a] 'mark-whole-buffer)
;; (global-set-key [?\A-z] 'undo)
;; (global-set-key [?\A-l] 'goto-line)
;; (global-set-key [?\A-m] 'iconify-frame)
;; (global-set-key [?\A-n] 'new-frame)

;; The value is in 1/10pt, so 100 gives us 10pt
;; see: http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; Pretty / larger font
;; (when (eq system-type 'darwin)
;;   (set-face-font 'default "FixedSCDZ")
;;   (set-face-attribute 'default nil :height 130))

;; you can use M-x list-faces-display to display
;; all the faces you'd like to customize

;; default font

(when (eq system-type 'darwin)

  ;; anti-aliasing for fonts.
  (setq ns-antialias-text t)

  ;; default Latin font (e.g. Consolas)
  ;; (set-face-attribute 'default nil :family "Consolas")
  ;; (set-face-font 'default "Monaco 12")
  ;; (set-face-font 'default "Menlo 12")
  (set-face-font 'default "Consolas 13")
  ;; (set-face-attribute 'default nil :family "Ubuntu Mono" :height 130)
  ;; (set-face-font 'default "FixedSCDZ")
  ;; (set-face-attribute 'default nil :family "Source Code Pro"
  ;;                     :weight 'ExtraLight :height 120)

  ;; (set-face-font 'default "Ubuntu Mono 14")
  ;; (set-face-attribute 'default nil :height 120)
  ;; (set-face-font 'default "Helvetica Monospaced Pro-12")
  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  ;; (set-face-attribute 'default nil :height 120)
  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  ;; you may want to add different for other charset in this way.
  )


;; font-face for the mode-line
;; (set-face-font 'mode-line "FixedSCDZ")
;; (set-face-font 'mode-line-buffer-id "FixedSCDZ")
;; (set-face-font 'mode-line-emphasis "FixedSCDZ")
;; (set-face-font 'mode-line-highlight "FixedSCDZ")
;; (set-face-font 'mode-line-inactive "FixedSCDZ")
;; (set-face-attribute 'mode-line nil :height 130)
;; (set-face-attribute 'mode-line-buffer-id nil :height 130)
;; (set-face-attribute 'mode-line-emphasis nil :height 130)
;; (set-face-attribute 'mode-line-highlight nil :height 130)
;; (set-face-attribute 'mode-line-inactive nil :height 130)
(setq line-height 1.0)


;; on a mac, remove the visibile-bell stuff
(setq visible-bell nil)

;; path setup

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. If you're using homebrew modifying the PATH is essential.
(setenv "PATH" (shell-command-to-string "/bin/bash -l -c 'echo -n $PATH'"))
(push "/usr/local/bin" exec-path)
(push "/usr/local/git/bin" exec-path)

;; flymake causes my emacs on MacOS X to hang
(setq flymake-gui-warnings-enabled nil)

(provide 'gauss-machine)

;;; gauss-machine.el ends here
