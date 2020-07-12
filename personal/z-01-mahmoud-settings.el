;; el-get
(setq el-get-notify-type 'message)
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
(require 'server)
(unless (server-running-p)
  (server-start))


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


;; midnight mode
(require 'midnight)
;;kill buffers if they were last disabled more than this seconds ago
(setq clean-buffer-list-delay-special 900)

(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable
   clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

;; run clean-buffer-list every 2 hours
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps
      '("^.*$"))

;; keep these buffer untouched
;; prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")

(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*"
         "*scratch*"
         "*Python*"
         "*Pymacs*")
       clean-buffer-list-kill-never-buffer-names-init))

;; prevent append multiple times
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")

;; append to *-init instead of itself
;; (setq clean-buffer-list-kill-never-regexps
;;       (append '("^\\*EMMS Playlist\\*.*$")
;;               clean-buffer-list-kill-never-regexps-init))

;; helm settings
(setq helm-candidate-number-limit 50)
(setq helm-ff-candidate-number-limit 200)
(setq helm-completion-style 'emacs)
;; uncomment for emacs 26.3
;; (setq completion-styles '(helm-flex))
;; for emacs 27+
(setq completion-styles '(flex))
