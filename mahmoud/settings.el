
;; emacs-starter-kit sources this file
;; don't put any requires from marmalade as they haven't been
;; added to the load-path yet.
(setq show-trailing-whitespace t)
(setq-default show-trailing-whitespace t)

;; untabify when you whitespace-cleanup
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00434.html
(defadvice whitespace-cleanup (after whitespace-untabify activate compile)
  (save-restriction
    (widen)
    (untabify (point-min) (point-max))))


;; Start the emacsclient server
(server-start)


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
