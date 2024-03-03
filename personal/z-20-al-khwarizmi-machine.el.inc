;;; z-20-gauss-machine.el --- description -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:
;;
;; Settings for macbook pro (codename: gauss)
;;

;;; Code:
;;; package --- Summary

(defun hyperify (prompt)
  (let ((e (read-event)))
    (vector (if (numberp e)
                (logior (ash 1 24) e)
              (if (memq 'hyper (event-modifiers e))
                  e
                (add-event-modifier "H-" e))))))

(defun superify (prompt)
  (let ((e (read-event)))
    (vector (if (numberp e)
                (logior (ash 1 23) e)
              (if (memq 'super (event-modifiers e))
                  e
                (add-event-modifier "s-" e))))))


(defun add-event-modifier (string e)
  (let ((symbol (if (symbolp e) e (car e))))
    (setq symbol (intern (concat string
                                 (symbol-name symbol))))
    (if (symbolp e)
        symbol
      (cons symbol (cdr e)))))

;; (keymap-set local-function-key-map "C-c h" 'hyperify)

;; if we're on WSL
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))


  ;(define-key local-function-key-map (kbd "<f13>") 'event-apply-super-modifier)
  (define-key local-function-key-map (kbd "<Tools>") 'superify)
  ; (define-key local-function-key-map (kbd "<Tools>") 'event-apply-surper-modifier)
  ;; excellent solution for copy & paste from

  (defun wsl-clipboard-to-string ()
    "Return Windows clipboard as string."
    (let ((coding-system-for-read 'dos))
      (shell-command-to-string "pbpaste.exe")
      ;; (substring; remove added trailing \n
      ;;  (shell-command-to-string "powershell.exe -Command Get-Clipboard") 0 -1)
      ))

  (defun wsl-paste-from-clipboard (arg)
    "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
    (interactive "P")
    (let ((clip (wsl-clipboard-to-string)))
      (insert clip)
      (if arg (kill-new clip))))


  (defun mahmoudimus/shell-command-on-str (cmd &optional str)
    "Insert result of calling CMD with STR as input.

STR is current-kill if unspecified.
"
    (interactive (list (read-shell-command "Shell command on region: ")))
    (setq str (or str
                  (current-kill 0)))
    (insert (with-temp-buffer
              (insert str)
              (shell-command-on-region (point-min) (point-max) cmd nil 'replace)
              (buffer-string))))


  (defun mahmoudimus/wsl-update-clip (&rest _args)
    (interactive)
    (mahmoudimus/shell-command-on-str "clip.exe"))


  (advice-add 'yank :override #'wsl-paste-from-clipboard)
  (advice-add 'kill-new :after #'mahmoudimus/wsl-update-clip)


  ;; https://emacs.stackexchange.com/a/40533/2082

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
  (global-set-key (kbd "s-z") 'undo)


  ;; for terminal use case use the function key as a super
  ;; mac switch meta key

  ;; Make alt be meta instead of command, so we can map command stuff to
  ;; normal cut and paste.
  ;; (setq mac-command-modifier 'alt
  ;;       mac-option-modifier 'meta)

  ;; Map command-x,c,v to cut, copy, paste
  (global-set-key [s-x] 'clipboard-kill-region)
  (global-set-key [s-c] 'clipboard-kill-ring-save)
  (global-set-key [s-v] 'clipboard-yank)

  ;; More mac shortcuts.
  (global-set-key [s-a] 'mark-whole-buffer)
  (global-set-key [s-z] 'undo)
  (global-set-key [s-l] 'goto-line)
  (global-set-key [s-m] 'iconify-frame)
  (global-set-key [s-n] 'new-frame)

  ;; The value is in 1/10pt, so 100 gives us 10pt
  ;; see: http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
  ;; Pretty / larger font
  ;; (when (eq system-type 'darwin)
  ;;   (set-face-font 'default "FixedSCDZ")
  ;;   (set-face-attribute 'default nil :height 130))

  ;; you can use M-x list-faces-display to display
  ;; all the faces you'd like to customize

  ;; default font

  (when (and (not (is-in-terminal)))


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
    ;; (setq m-font-face '"Operator SSm")
    (text-scale-set 0)

    ;; (setq m-font-face '"Unifont")
    ;; (setq m-font-face '"Relative") ;; does not do what you think it does!

    ;; if you set anti-aliasing, remember that bitmap fonts won't look good
    ;; bitmap fonts!
    (setq m-font-face-bitmap '"Unifont") ;; or Unifont-JP
    (setq m-font-face-bitmap '"VT323")
    (setq m-font-face-bitmap '"Fixedsys Excelsior")
    (setq m-font-face-bitmap '"FixedSCDZ")
    (setq m-font-face-bitmap '"fixed-7x14")
    (setq m-font-face-bitmap '"fixed-9x15")
    (setq m-font-face-bitmap '"SMILEBASIC")
    (setq m-font-face-bitmap '"SMILEBASIC_B")
    (setq m-font-face-bitmap '"Spleen 8x16")
    (setq m-font-face-bitmap '"Spleen 12x24")
    (setq m-font-face-bitmap '"Spleen 16x32")
    (setq m-font-face-bitmap '"Spleen 32x64")
    (setq m-font-face-bitmap '"Aux Mono")
    (setq m-font-face-bitmap '"Envoy Code B 10pt")
    (setq m-font-face-bitmap '"IBM 3270")
    (setq m-font-face-bitmap '"IBM 3270 Condensed")
    (setq m-font-face-bitmap '"IBM 3270 Semi-Condensed")
    (setq m-font-face-bitmap '"DSE Typewriter Bitmap")
    (setq m-font-face-bitmap '"fn0t")
    (setq m-font-face-bitmap '"Mx437 Cordata PPC-400") ;; no smart quotes
    (setq m-font-face-bitmap '"MxPlus ToshibaSat 8x14")
    (setq m-font-face-bitmap '"MxPlus ToshibaSat 8x16")
    (setq m-font-face-bitmap '"MxPlus ToshibaSat 9x14")
    (setq m-font-face-bitmap '"MxPlus ToshibaSat 9x16")
    (setq m-font-face-bitmap '"MxPlus ToshibaTxL1 8x16") ;; bold
    (setq m-font-face-bitmap '"MxPlus ToshibaTxL2 8x16") ;; regular
    (setq m-font-face-bitmap '"Sweet16")
    (setq m-font-face-bitmap '"Tamzen")
    (setq m-font-face-bitmap '"Terminus (TTF)")
    (setq m-font-face-bitmap '"CodingFontTobi")


    (setq m-font-face '"Consola Mono")
    (setq m-font-face '"Fira Code")
    (setq m-font-face '"Lekton")
    (setq m-font-face '"Luculent")
    (setq m-font-face '"Fantasque Sans Mono")
    (setq m-font-face '"Space Mono")
    (setq m-font-face '"Lilex")

    ;;menlo


    ;; trusty
    (setq m-font-face '"Overpass Mono")
    (setq m-font-face '"Consolas ligaturized v3 Regular")
    (setq m-font-face '"PT Mono")
    (setq m-font-face '"Input Mono")
    (setq m-font-face '"Iosevka")
    (setq m-font-face '"Rec Mono Linear")
    (setq m-font-face '"Bmono")
    (setq m-font-face '"Maison Neue Mono")
    (setq m-font-face '"Panic Sans")
    (setq m-font-face '"Pitch Sans")


    ;; thin
    (setq m-font-face-thin '"Anonymous Pro")
    (setq m-font-face-thin '"Share Tech Mono")
    (setq m-font-face-thin '"Pragmata Pro Mono")
    (setq m-font-face-thin '"Input Mono Compressed") ;; thinnest
    (setq m-font-face-thin '"Input Mono Condensed")
    (setq m-font-face-thin '"Input Mono Narrow")
    (setq m-font-face-thin '"mplus 1m Nerd Font")
    (setq m-font-face-thin '"NanumGothicCoding")
    (setq m-font-face-thin '"Sudo Variable")

    ;; Nerd
    (setq m-font-face-nerd '"TamzenForPowerline")
    (setq m-font-face-nerd '"TerminessTTF Nerd Font")
    (setq m-font-face-nerd '"MonoLisa Nerd Font Mono")
    (setq m-font-face-nerd '"BlexMono Nerd Font Mono")
    (setq m-font-face-nerd '"JetBrainsMono Nerd Font Mono")
    (setq m-font-face-nerd '"Envy Code R for Powerline")
    (setq m-font-face-nerd '"CaskaydiaCove Nerd Font")


    ;; serif-mono
    (setq m-font-face-serif-mono '"Courier New") ;; does not diff between O and 0
    (setq m-font-face-serif-mono '"Courier Prime") ;; does not diff between O and 0
    (setq m-font-face-serif-mono '"Go Mono")
    (setq m-font-face-serif-mono '"Verify Serif Mono")
    (setq m-font-face-serif-mono '"Courier Prime Code")
    (setq m-font-face-serif-mono '"DSE Typewriter AH")
    (setq m-font-face-serif-mono '"DSE Typewriter LH")
    (setq m-font-face-serif-mono '"DSE Typewriter NH")
    (setq m-font-face-serif-mono '"Triplicate T4c")
    (setq m-font-face-serif-mono '"Pitch")
    (setq m-font-face-serif-mono '"Terminal Land Mono")

    ;; Fun
    (setq m-font-face '"Code New Roman")
    (setq m-font-face '"MonacoB")
    (setq m-font-face '"Azeret Mono")
    (setq m-font-face '"Söhne Mono")
    (setq m-font-face '"Gintronic")
    (setq m-font-face '"Spot Mono")
    (setq m-font-face '"Gopher Mono")
    (setq m-font-face '"B612 Mono") ;; B612 with slashed 0!
    (setq m-font-face '"Besixdouze Code") ;; B612 with slashed 0!
    (setq m-font-face '"Calling Code")
    (setq m-font-face '"Monoflow")
    (setq m-font-face '"Monoid")
    (setq m-font-face '"DM Mono")
    (setq m-font-face '"Dank Mono")
    (setq m-font-face '"iA Writer Duospace")
    (setq m-font-face '"Operator Mono SSm Lig")
    (setq m-font-face '"Drafting Mono")
    (setq m-font-face '"Aardvark Fixed")
    (setq m-font-face '"Terminal Land Mono Sans")
    (setq m-font-face '"Cascadia Code PL")
    (setq m-font-face '"Victor Mono")
    (setq m-font-face '"IBM Plex Mono")
    (setq m-font-face '"Native")
    (setq m-font-face '"Terminex")
    (setq m-font-face '"JuliaMono")
    (setq m-font-face '"MonoLisa")
    (setq m-font-face '"Berkeley Mono Variable")
    (setq m-font-face '"Mahmoudimus Iosevka v2")

    (set-face-font 'default m-font-face) ;; can be light as well instead of XLight
    (set-face-attribute 'default nil
                        ;; :family m-font-face
                        :weight 'normal
                        :height 150)
    (set-face-font 'mode-line m-font-face-nerd)
    (set-face-attribute 'mode-line nil
                        ;; :family m-font-face
                        :weight 'normal
                        :height 130)
    (set-face-font 'mode-line-inactive m-font-face-nerd)
    (set-face-attribute 'mode-line-inactive nil
                        ;; :family m-font-face
                        :weight 'Light
                        :height 130)
    ;; (set-face-font 'mode-line-inactive "SFMono Nerd Font 10")
    ;; Use dark theme for title bar. This works only on Emacs 26.1 and above.

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
  ;; on a mac, remove the visibile-bell stuff
  (setq visible-bell nil)



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
  (setq insert-directory-program "/usr/local/bin/gls")))

(provide 'khwarizmi-machine)

;;; gauss-machine.el ends here
