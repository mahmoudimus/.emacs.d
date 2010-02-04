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


;; Remap ctrl-x ctrl-p to fill-paragraph so it doesn't conflict with
;; meta-q in xmonad
;; (global-set-key [ (control x) (p) ] 'fill-paragraph)

;; The value is in 1/10pt, so 100 gives us 10pt
;; see: http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; Pretty / larger font
(when (eq system-type 'darwin)
     (set-face-font 'default "Monaco")
     (set-face-attribute 'default nil :height 110))
(when (eq system-type 'gnu/linux)
    (set-face-font 'default "Menlo")
    (set-face-attribute 'default nil :height 90))


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
(global-set-key (kbd "C-c t a b e"    ) 'elscreen-create)
(global-set-key (kbd "C-c t a b d"  ) 'elscreen-kill)  


;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "C-c b T") 'elscreen-previous) 
(global-set-key (kbd "C-c b t")  'elscreen-next) 


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
(define-key ac-mode-map (kbd "<C-tab>") 'auto-complete)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
;; ------------------------------------
;; smart-operator
;; ------------------------------------
(require 'smart-operator)

