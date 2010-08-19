;; Pretty colors
(load "~/.emacs.d/vendor/color-theme-6.6.0/themes/color-theme-library.el")
(load-file "~/.emacs.d/vendor/color-theme-6.6.0/themes/color-theme-railscasts.el")
(color-theme-railscasts)

;; yasnippets
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; emacs-textmate
;; (adds better ", ], \) etc pairing).
(load-file "~/.emacs.d/vendor/emacs-textmate/textmate.el")
(textmate-mode) ;; TODO: Should I add hooks? Nah, I want it on almost always...

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
;; god this is fucking annoying!!
(setq scroll-step 1)


;; Remap ctrl-x ctrl-p to fill-paragraph so it doesn't conflict with
;; meta-q in xmonad
;; (global-set-key [ (control x) (p) ] 'fill-paragraph)



(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

;; Start the emacsclient server
;;(server-start)

(add-hook 'after-init-hook 'server-start)

;; Add column numbers to the status bar.
(column-number-mode)

;; Bind keys to make recording/recalling macros easier
(global-set-key (read-kbd-macro "A-M-m")
                'start-kbd-macro)

(global-set-key (read-kbd-macro "A-M-x")
                'call-last-kbd-macro)

;; Bind keys to allow for getting to the start of the next word
;; sort of like vi's 'w' in command-mode
(global-set-key (kbd "M-F") '(lambda ()
                               (interactive)
                               (forward-word 2)
                               (backward-word 1)))

;; Make command Meta instead of Alt
(setq ns-command-modifier (quote meta))

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

;; No tabs damnit.
(setq-default indent-tabs-mode nil)

;; Make tabs stand out
(defface extra-whitespace-face
   '((t (:background "pale green")))
   "Used for tabs and such.")
(defvar my-extra-keywords
   '(("\t" . 'extra-whitespace-face)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(add-hook 'text-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(add-hook 'python-mode-hook
          (lambda () (font-lock-add-keywords nil my-extra-keywords)))
(font-lock-add-keywords nil my-extra-keywords)


;; Tab size to 4
(setq-default tab-width 4)

;; change regexp highlight face to something other than yellow
(defface highlight-regexp-face
  '((t (:foreground "#B28BD6")))
  "face for highlight-regexp")

;; ---------------------------------------
;; load elscreen
;; ---------------------------------------
(load "elscreen" "ElScreen" t)

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "C-c t a b e") 'elscreen-create)
(global-set-key (kbd "C-c t a b d") 'elscreen-kill)


;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "C-M-_") 'elscreen-previous)
(global-set-key (kbd "C-M-+") 'elscreen-next)


;; --------------------------------------
;; set cursor type
;; -------------------------------------
;; don't have that fucking ugly block, since we're always
;; in insert mode, let's just keep it like Vim's insert
;; mode
;; (setq default-frame-alist '((cursor-type . (bar . 10))))

;; ------------------------------------
;; hotkeys
;; ------------------------------------


;; ------------------------------------
;; transpose buffers
;; ------------------------------------
(setq swapping-buffer nil)
(setq swapping-window nil)
(defun swap-buffers-in-windows ()
  "Swap buffers between two windows"
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-buffer nil)
        (setq swapping-window nil))
    (progn
      (setq swapping-buffer (current-buffer))
      (setq swapping-window (selected-window))
      (message "Buffer and window marked for swapping."))))
(global-set-key (kbd "C-c s") 'swap-buffers-in-windows)

;; ------------------------------------
;; auto-complete
;; ------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-auto-start 2)
(setq ac-dwim t)
(global-auto-complete-mode t)
(define-key ac-mode-map (kbd "<C-tab>") 'auto-complete)
;; (define-key ac-mode-map (kbd "M-") 'auto-complete)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
;; ------------------------------------
;; smart-operator
;; ------------------------------------
(require 'smart-operator)
;; ------------------------------------
;; desktop/emacs session saves
;; note: vim, i love you:(
;; ------------------------------------
(require 'slick-desktop)
