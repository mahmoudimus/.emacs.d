;;; starter-kit-python.el --- Some helpful Python code
;;
;; Part of the Emacs Starter Kit (Added by Mahmoud Abdelkader
;;<mahmoud@linux.com>

;; load the python directory files
;; and set the variable
(setq python-files-dir (concat dotfiles-dir "languages/python/"))

;; let's set the python path correctly as well
(setenv "PYTHONPATH" (concat python-files-dir
                             (concat path-separator
                                     (getenv "PYTHONPATH"))))

;; load the python subdirectory
(add-to-list 'load-path python-files-dir)
;; load python/Pymacs
(add-to-list 'load-path (concat python-files-dir "Pymacs/"))
;; load python/rope
(add-to-list 'load-path (concat python-files-dir "rope/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;There are TWO python modes
; 1) Tim Peter's python-mode.el -- this is the standard/legacy way
; 2) Dave Love's python.el -- this is when Dave Love got frustrated
; that python-mode wasn't accepting his patches
;
;The following directory has a .nosearch file in it therefore it not in
;the current load-path and the default python-mode will be used instead
;The following loads Dave Love's python mode:
;(add-to-list 'load-path "~/.emacs.d/dave-loves-python-mode"')

;; enable the python mode (legacy) yay barry warsaw! <3 umd graduates!
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                  interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(setq pymacs-load-path (append (list (concat python-files-dir "rope/")
                                     (concat python-files-dir "ropemode/")
                                     (concat python-files-dir "Pymacs/")
                                     (concat python-files-dir "pycomplete/")
                                     (concat python-files-dir "ropemacs/"))
                                nil))
;;enable pymacs
(require 'pymacs)
(require 'python-mode)
(require 'auto-complete)
(require 'auto-complete-config)

;; python indentation hooks
;; TODO: clean this up a bit by hooking into a dispatcher function
(add-hook 'python-mode-hook
      (lambda ()
        (set-variable 'py-indent-offset 4)
        (set-variable 'py-continuation-offset 0)
        ;(set-variable 'py-smart-indentation nil)
        (set-variable 'indent-tabs-mode nil)
        (define-key py-mode-map (kbd "C-RET") 'newline-and-indent)
        (local-set-key (kbd "<M-S-iso-lefttab>") 'mahmoud-force-indent)
        (require 'pycomplete)
        ;(define-key py-mode-map [tab] 'yas/expand)
        ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
        ;(smart-operator-mode-on)
      ))

(defun mahmoud-force-indent (&optional arg)
    (interactive "P")
    (insert-tab arg))

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python auto-fill comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on by defualt?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
       (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(add-hook 'python-mode-hook
          (lambda ()
        (auto-complete-mode t)))
        ;(set (make-local-variable 'ac-sources)
        ;(append ac-sources '(ac-source-rope)))
        ;(set (make-local-variable 'ac-find-function) 'ac-python-find)
        ;(set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
        ;(set (make-local-variable 'ac-auto-start) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Ryan's python specific tab completion
  ; Try the following in order:
  ; 1) Try a yasnippet expansion without autocomplete
  ; 2) If at the beginning of the line, indent
  ; 3) If at the end of the line, try to autocomplete
  ; 4) If the char after point is not alpha-numerical, try autocomplete
  ; 5) Try to do a regular python indent.
  ; 6) If at the end of a word, try autocomplete.
;(define-key py-mode-map (kbd "TAB") 'yas/expand)
;(add-hook 'python-mode-hook
      ;(lambda ()
        ;(set (make-local-variable 'yas/trigger-fallback)
             ;'ryan-python-expand-after-yasnippet)))

;(defun ryan-indent ()
  ;"Runs indent-for-tab-command but returns t if it actually did an indent; nil otherwise"
  ;(let ((prev-point (point)))
    ;(indent-for-tab-command)
    ;(if (eql (point) prev-point)
        ;nil
      ;t)))

;(defun ryan-python-expand-after-yasnippet ()
  ;(interactive)
  ;;;2) Try indent at beginning of the line
  ;(let ((prev-point (point))
        ;(beginning-of-line nil))
    ;(save-excursion
      ;(move-beginning-of-line nil)
      ;(if (eql 0 (string-match "\\W*$" (buffer-substring (point) prev-point)))
          ;(setq beginning-of-line t)))
    ;(if beginning-of-line
        ;(ryan-indent)))
  ;;;3) Try autocomplete if at the end of a line, or
  ;;;4) Try autocomplete if the next char is not alpha-numerical
  ;(if (or (string-match "\n" (buffer-substring (point) (+ (point) 1)))
          ;(not (string-match "[a-zA-Z0-9]" (buffer-substring (point) (+ (point) 1)))))
      ;(ac-start)
    ;;;5) Try a regular indent
    ;(if (not (ryan-indent))
        ;;;6) Try autocomplete at the end of a word
        ;(if (string-match "\\W" (buffer-substring (point) (+ (point) 1)))
            ;(ac-start)))))

;;; End Tab completion

;;Workaround so that Autocomplete is by default is only invoked explicitly,
;;but still automatically updates as you type while attempting to complete.
;(defadvice ac-start (before advice-turn-on-auto-start activate)
;  (set (make-local-variable 'ac-auto-start) t))
;(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;  (set (make-local-variable 'ac-auto-start) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load flymake lint runner
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (concat (getenv "HOME") "/bin/lintrunner.py") (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; add python hook when in python to activate flymake lint
(add-hook 'python-mode-hook (lambda() (flymake-mode t)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

;;; django mode
;; load django mode
(add-to-list 'load-path (concat python-files-dir "django-mode/"))
;;(require 'django-html-mode)
;;(require 'django-mode)

;; load virtualenv
(add-to-list 'load-path (concat dotfiles-dir "/libs/virtualenv.el"))


(provide 'starter-kit-python)
