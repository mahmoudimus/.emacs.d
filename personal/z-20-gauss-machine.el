;;; z-20-gauss-machine.el --- description -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:
;;
;; Settings for macbook pro (codename: gauss)
;;

;;; Code:
;;; package --- Summary

;; setup key bindings to allow for both super and hyper to have useful
;; bindings. also paper over the differences between the defaults in
;; the stock and railway cats distributions.
(when (eq system-type 'darwin)
  ;; mac osx, use ns-* settings to distiguish between the flavors of emacs available.
  ;; L-Command -> Super
  ;; L-Option -> Meta
  ;; R-Option -> Hyper
  ;; Fn -> Alt  ;; \A (function = alt!)
  ;; https://emacs.stackexchange.com/a/40533/2082
  ;; (defvaralias 'mac-allow-anti-aliasing 'ns-antialias-text)
  ;; (defvaralias 'mac-command-modifier 'ns-command-modifier)
  ;; (defvaralias 'mac-right-command-modifier 'ns-right-command-modifier)
  ;; (defvaralias 'mac-control-modifier 'ns-control-modifier)
  ;; (defvaralias 'mac-right-control-modifier 'ns-right-control-modifier)
  ;; (defvaralias 'mac-option-modifier 'ns-option-modifier)
  ;; (defvaralias 'mac-right-option-modifier 'ns-right-option-modifier)
  ;; (defvaralias 'mac-function-modifier 'ns-function-modifier)
  ;; (defvaralias 'ns-option-modifier 'ns-alternate-modifier)
  ;; (defvaralias 'ns-right-option-modifier 'ns-right-alternate-modifier)

  ;; on Mac OS X, use Option keys as Meta and file polling for auto-revert
  (setq auto-revert-use-notify nil ;; OS X does not support file notifications
        mac-option-modifier 'meta ;; use Option key as Meta
        mac-right-option-modifier 'none ;; right Option uses left's mapping
        mac-right-command-modifier 'hyper
        mac-function-modifier 'alt
        mac-command-modifier 'super)) ;; keep Super key as is

(when (eq window-system 'ns)
  (setq mac-option-modifier 'meta ;; use Option key as Meta
        mac-right-option-modifier 'none ; right Option uses left's mapping
        mac-command-modifier 'super
        ns-option-modifier 'meta
        ns-right-command-modifier 'hyper
        ns-right-option-modifier 'none  ; Keep right option for accented input
        ns-function-modifier 'alt
        ns-command-modifier 'super)) ;; keep Super key as is


;; revert Command keys in Emacs Mac Port to match Emacs for Mac OS X bindings
(when (eq window-system 'mac)
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'none
        mac-right-command-modifier 'hyper
        mac-function-modifier 'alt
        mac-command-modifier 'super)

  (if (boundp 'ns-use-native-fullscreen)
      (progn
        (setq ns-use-native-fullscreen t)))

  (global-set-key (kbd "s-'") 'next-multiframe-window)
  (global-set-key (kbd "s-,") 'customize)
  (global-set-key (kbd "s-`") 'other-frame)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-c") 'kill-ring-save) ;; ns-copy-including-secondary
  (global-set-key (kbd "s-d") 'isearch-repeat-backward)
  (global-set-key (kbd "s-f") 'isearch-forward)
  (global-set-key (kbd "s-g") 'isearch-repeat-forward)
  (global-set-key (kbd "s-h") 'ns-do-hide-emacs) ;; done by default
  (global-set-key (kbd "s-j") 'exchange-point-and-mark)
  (global-set-key (kbd "s-k") 'kill-this-buffer)
  (global-set-key (kbd "s-l") 'goto-line)
  (global-set-key (kbd "s-m") 'iconify-frame)
  (global-set-key (kbd "s-n") 'make-frame)
  ;; (global-set-key (kbd "s-o") 'ns-open-file-using-panel) ;; no equivalent
  ;; (global-set-key (kbd "s-p") 'ns-print-buffer) ;; no equivalent
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-u") 'revert-buffer)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-w") 'delete-frame)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-y") 'yank) ;; ns-paste-secondary
  (global-set-key (kbd "s-z") 'undo))


;; for terminal use case use the function key as a super
;; mac switch meta key

;; Make alt be meta instead of command, so we can map command stuff to
;; normal cut and paste.
;; (setq mac-command-modifier 'alt
;;       mac-option-modifier 'meta)

;; Map command-x,c,v to cut, copy, paste
(global-set-key [?\A-x] 'clipboard-kill-region)
(global-set-key [?\A-c] 'clipboard-kill-ring-save)
(global-set-key [?\A-v] 'clipboard-yank)

;; More mac shortcuts.
(global-set-key [?\A-a] 'mark-whole-buffer)
(global-set-key [?\A-z] 'undo)
(global-set-key [?\A-l] 'goto-line)
(global-set-key [?\A-m] 'iconify-frame)
(global-set-key [?\A-n] 'new-frame)
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
  ;; -OperatorMono Nerd Font-normal-normal-norma
  ;; M-x describe-font
  ;; (setq m-font-face '"CozetteVector")
  (text-scale-set 0)
  (setq m-font-face '"Akkurat Mono")
  (setq m-font-face '"Operator SSm")
  (setq m-font-face '"Overpass Mono")
  (setq m-font-face '"CaskaydiaCove Nerd Font Mono")
  (setq m-font-face '"VT323")
  (setq m-font-face '"Unifont")
  (setq m-font-face '"Dank Mono")
  (setq m-font-face '"Dm Mono")
  (setq m-font-face '"Fira Code")
  (setq m-font-face '"Lekton")
  (setq m-font-face '"Luculent")
  (setq m-font-face '"Fantasque Sans Mono")
  (setq m-font-face '"Space Mono")
  (setq m-font-face '"Cousine")
  (setq m-font-face '"OperatorMono Nerd Font")
  (setq m-font-face '"NanumGothicCoding")
  (setq m-font-face '"JetBrainsMono Nerd Font")
  ;; (setq m-font-face '"RecMono Linear")

  (set-face-font 'default m-font-face) ;; can be light as well instead of XLight
  (set-face-attribute 'default nil
                      :family m-font-face
                      :weight 'normal
                      :height 150)
  (set-face-font 'mode-line m-font-face)
  (set-face-attribute 'mode-line nil
                      :family m-font-face
                      :weight 'normal
                      :height 130)
  (set-face-font 'mode-line-inactive m-font-face)
  (set-face-attribute 'mode-line-inactive nil
                      :family m-font-face
                      :weight 'Light
                      :height 130)
  ;; (set-face-font 'mode-line-inactive "SFMono Nerd Font 10")
  ;; Use dark theme for title bar. This works only on Emacs 26.1 and above.
  (when (not (version< emacs-version "26.1"))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))

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
                         (:foreground "blue"
                                      :slant normal
                                      :weight light
                                      :height 120
                                      :font "SF Mono"))))
                                      ;;:font "IBM Plex Mono Medium"))))
   '(neo-file-link-face ((t
                          (:foreground "slate gray"
                                       :weight medium
                                       :height 110
                                       :font "Source Code Pro"))))
                                       ;;:font "Menlo"))))
                                       ;;:font "Bront"))))
                                       ;;:font "PragmataPro Mono Liga"))))
                                       ;;:font "IBM Plex Mono ExtraLight"))))
   '(neo-root-dir-face ((t
                         (:foreground "deep sky blue"
                                      :weight medium
                                      :height 120
                                      :font "SF Mono")))))
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

(when (file-exists-p "/usr/local/bin/gls")
  (setq insert-directory-program "/usr/local/bin/gls"))

(provide 'gauss-machine)

;;; gauss-machine.el ends here
