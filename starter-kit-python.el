;;; starter-kit-python.el --- Some helpful Python code
;;
;; Part of the Emacs Starter Kit (Added by Mahmoud Abdelkader
;;<mahmoud@linux.com>

;; load the python directory files
;; and set the variable
(setq python-files-dir (concat dotfiles-dir "/python"))

;; load the python subdirectory
(add-to-list 'load-path python-files-dir)
;; load python/Pymacs
(add-to-list 'load-path (concat python-files-dir "/Pymacs"))
;; load python/rope
(add-to-list 'load-path (concat python-files-dir "/rope"))

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

;; enable the python mode (legacy)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;(setq pymacs-load-path '("~/.emacs.d/site-lisp/rope"|#
                         ;"~/.emacs.d/site-lisp/ropemode"
                         ;"~/.emacs.d/site-lisp/ropemacs"))
(require 'python-mode)

;; python indentation hooks
(add-hook 'python-mode-hook
      (lambda ()
        (set-variable 'py-indent-offset 4)
                    ;(set-variable 'py-smart-indentation nil)
        (set-variable 'indent-tabs-mode nil)
        (define-key py-mode-map (kbd "RET") 'newline-and-indent)
                    ;(define-key py-mode-map [tab] 'yas/expand)
                    ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
        ))

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;;enable pymacs
(require 'pymacs)

;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

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
     . (lambda ()y
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
        (auto-complete-mode 1)
        ;(set (make-local-variable 'ac-sources)
        ;(append ac-sources '(ac-source-rope)))
        (set (make-local-variable 'ac-find-function) 'ac-python-find)
        (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
        (set (make-local-variable 'ac-auto-start) nil)))

;;Ryan's python specific tab completion
(defun ryan-python-tab ()
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))

(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

(define-key py-mode-map "\t" 'ryan-python-tab)
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
      (list "/home/mahmoud/bin/lintrunner.py" (list local-file))))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; add python hook when in python to activate flymake lint
(add-hook 'python-mode-hook (lambda() (flymake-mode t)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(provide 'starter-kit-python.el)
