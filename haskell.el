;; macbook pro
;; 15"

;; Make command Meta instead of Alt
;; (setq ns-command-modifier (quote meta))
;; Make option Super instead of Alt
;; (setq ns-alternate-modifier (quote meta))
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
(when (eq system-type 'darwin)
  ;; default font
;;   (set-face-font 'default "Monaco")
;;   (set-face-attribute 'default nil :height 110)
   (set-face-font 'default "Helvetica Monospaced Pro-12")
   ;; font-face for the mode-line
   (set-face-font 'mode-line "FixedSCDZ")
   (set-face-font 'mode-line-buffer-id "FixedSCDZ")
   (set-face-font 'mode-line-emphasis "FixedSCDZ")
   (set-face-font 'mode-line-highlight "FixedSCDZ")
   (set-face-font 'mode-line-inactive "FixedSCDZ")
   (set-face-attribute 'mode-line nil :height 130)
   (set-face-attribute 'mode-line-buffer-id nil :height 130)
   (set-face-attribute 'mode-line-emphasis nil :height 130)
   (set-face-attribute 'mode-line-highlight nil :height 130)
   (set-face-attribute 'mode-line-inactive nil :height 130)
   )
(setq line-height 1.0)

;; disable flymake-mode on the mac for now until I can get the script/linters running
(setq flymake-mode nil)

;; on a mac, remove the visibile-bell stuff
(setq visible-bell nil)

;; path setup
(setenv "PATH" (shell-command-to-string "/bin/bash -l -c 'echo -n $PATH'"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/local/git/bin")))


;; flymake fucking hangs mac os x
(setq flymake-gui-warnings-enabled nil)
