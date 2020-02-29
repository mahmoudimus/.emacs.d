;;; package --- Summary
;; This is my MacBook Pro 17"

;;; Commentary:

;;; Code:

;; Make command Super
(setq ns-command-modifier (quote super))
(setq mac-command-modifier 'super)
;; Make option Meta
(setq ns-alternate-modifier (quote meta))
(setq mac-option-modifier 'meta)

;; Make the function key Alt
(setq ns-function-modifier (quote alt))
(setq mac-function-modifier 'alt)

;; for terminal use case use the function key as a super
;; mac switch meta key

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

(when (and (not (is-in-terminal))
           (eq system-type 'darwin))

  ;; anti-aliasing for fonts.
  (setq ns-antialias-text t)
  ;; font setting
  ;;(create-fontset-from-ascii-font "M+ 1mn:pixelsize=14:weight=normal:slant=normal:spacing=m" nil "mplus1mn")
  ;;(set-fontset-font "fontset-mplus1mn" 'unicode
                    ;;(font-spec :family "M+ 1mn" :spacing 'm :size 14)
                    ;;nil 'prepend)
  ;; (add-to-list 'default-frame-alist '(font . "fontset-mplus1mn"))

  ;; default Latin font (e.g. Consolas)
  ;; (set-face-attribute 'default nil :family "Consolas")
  ;; (set-face-font 'default "Monaco 12")
  ;; (set-face-font 'default "Menlo 12")
  ;; (set-face-font 'default "Consolas 13")
  ;; (set-face-attribute 'default nil :family "Ubuntu Mono" :height 130)
  ;; (set-face-font 'default "FixedSCDZ")
  ;; (set-face-attribute 'default nil :family "Source Code Pro"
  ;;                     :weight 'ExtraLight :height 120)

  ;; ubuntu mono doesn't support greek glyphs :-(
  ;; (set-face-font 'default "Ubuntu Mono 16")
  ;; (set-face-font 'default "Monaco 12")
  ;; (set-face-font 'default "Menlo 12")
  ;; (set-face-font 'default "Operator Mono Lig Book 14")
  ;; (set-face-font 'default "-*-M+ 2m-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
  ;; (set-face-font 'default "Helvetica Monospaced Pro-12")
  ;; (set-face-font 'default "Terminus (TTF)-13")
  (set-face-font 'mode-line "Operator Mono Lig 12")
  (set-face-font 'mode-line-inactive "Operator Mono Lig 10")
  ;; (set-face-font 'mode-line-inactive "SFMono Nerd Font 10")

  ;;(set-face-attribute 'default nil :height 130)
  (setq line-spacing nil)
  ;; (setq line-height t)
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
  ;; (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))
  ;; you may want to add different for other charset in this way.

  (custom-set-faces
   ;; '(cursor ((t (:background "gold" :foreground "#151718"))))
   ;; '(mode-line ((t (:background "black" :foreground "#4499FF"))))
   '(neo-dir-link-face ((t
                         (:foreground "deep sky blue"
                                      :slant normal
                                      :weight normal
                                      :height 140
                                      ;;:font "SF Mono Medium"))))
                                      :font "IBM Plex Mono Medium"))))
   '(neo-file-link-face ((t
                          (:foreground "deep sky grey"
                                       :weight normal
                                       :height 140
                                       :font "Menlo"))))
                                       ;;:font "Bront"))))
                                       ;;:font "PragmataPro Mono Liga"))))
                                       ;;:font "IBM Plex Mono ExtraLight"))))
   '(neo-root-dir-face ((t
                         (:foreground "light blue"
                                      :height 140
                                      :weight normal)))))
                                      ;;:weight bold)))))

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

(require 'elisp-format)


;; on a mac, remove the visibile-bell stuff
(setq visible-bell nil)

;; path setup

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. If you're using homebrew modifying the PATH is essential.
;; (setenv "PATH" (shell-command-to-string "/bin/bash -l -c 'echo -n $PATH'"))

;; (setenv "PATH" (shell-command-to-string "/bin/zsh -l -c 'source ~/.zshrc; echo -n $PATH'"))

;; (push "/usr/local/bin" exec-path)
;; (push "/usr/local/git/bin" exec-path)

;; flymake causes my emacs on MacOS X to hang
;; (setq flymake-gui-warnings-enabled nil)

(provide 'gauss-machine)

;;; gauss-machine.el ends here
