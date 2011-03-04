;;;;;;;;;;;;;;;;;;; Pretty colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/libs/color-theme-6.6.0/themes/color-theme-library.el")

;; zen and art
;; (load-file "~/.emacs.d/libs/color-theme-6.6.0/themes/color-theme-zen-and-art.el")
;  (color-theme-zen-and-art)

;; railscasts
;; (load-file "~/.emacs.d/libs/color-theme-6.6.0/themes/color-theme-railscasts.el")
;; (color-theme-railscasts)

;; twilight color theme
;; (load-file "~/.emacs.d/libs/color-theme-6.6.0/themes/color-theme-twilight.el")
;; (color-theme-twilight)

;; zenburn
(load-file "~/.emacs.d/libs/color-theme-6.6.0/themes/color-theme-zenburn.el")
(color-theme-zenburn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ya - snippets ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ya - snippets ;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/libs/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/libs/yasnippet/snippets")

;; textmate mode
(load-file "~/.emacs.d/libs/textmate.el/textmate.el")
(textmate-mode)

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
(global-set-key (kbd "s-F") '(lambda ()
                               (interactive)
                               (forward-word 2)
                               (backward-word 1)))

;; make C-N forward-paragraph and C-P backward-paragraph
(global-set-key (kbd "s-N") 'forward-paragraph)
(global-set-key (kbd "s-P") 'backward-paragraph)

(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-b") 'backward-word)


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
(add-to-list 'load-path (concat dotfiles-dir "/libs/apel"))
(add-to-list 'load-path (concat dotfiles-dir "/libs/elscreen"))
(load "elscreen" "ElScreen" t)

;; F9 creates a new elscreen, shift-F9 kills it
(global-set-key (kbd "C-c t a b e") 'elscreen-create)
(global-set-key (kbd "C-c t a b d") 'elscreen-kill)


;; Windowskey+PgUP/PgDown switches between elscreens
(global-set-key (kbd "s-{") 'elscreen-previous)
(global-set-key (kbd "s-}") 'elscreen-next)


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
;; this is actually loaded from python start up stuff
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
(add-to-list 'load-path (concat dotfiles-dir "/libs/smart-operator"))
(require 'smart-operator)
;; ------------------------------------
;; desktop/emacs session saves
;; note: vim, i love you:(
;; ------------------------------------
;;(add-to-list 'load-path (concat dotfiles-dir "/libs/slick-desktop"))
;;(require 'slick-desktop)
;;
;; Execute "M-x desktop-save" once and it will update whenever auto-save
;; occurs (and on every exit?)
(require 'desktop)
(setq desktop-path '("~/.emacs.d/sessions"))
(setq desktop-dirname "~/.emacs.d/sessions")
(setq desktop-base-file-name "emacs.session")
(desktop-save-mode 1)
(add-hook 'auto-save-hook
          (lambda () (desktop-save-in-desktop-dir)))

;; Save a bunch of variables to the desktop file for lists specify the
;; len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; Get rid of the stupid <2> and <3> filename uniqification
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ------------------------------------
;; org mode stuff
;; ------------------------------------
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; ------------------------------------
;; revert all buffers
;; ------------------------------------
;; From http://blog.plover.com/prog/revert-all.html
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

;; ------------------------------------
;; require magit mode
;; ------------------------------------
(require 'magit)
