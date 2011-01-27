(setenv "PYTHONPATH" "/pluto/pluto:/pluto/pycloud")
(setenv "PATH" "/home/mahmoud/bin:/home/mahmoud/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games")

;(when (load "flymake" t)
  ;(defun flymake-pylint-init ()
    ;(let* ((temp-file (flymake-init-create-temp-buffer-copy
                       ;'flymake-create-temp-inplace))
           ;(local-file (file-relative-name
                        ;temp-file
                        ;(file-name-directory buffer-file-name))))
      ;(list "/home/mahmoud/bin/lintrunner.py" (list local-file))))

  ;(add-to-list 'flymake-allowed-file-name-masks
               ;'("\\.py\\'" flymake-pylint-init)))

;(add-hook 'python-mode-hook (lambda() (flymake-mode t)))

;; (setq default-frame-alist
;;       (append
;;        '((font
;;           . "-misc-fixed-medium-r-semicondensed--13-*-*-*-c-60-iso8859-1"))
;;        default-frame-alist))

;; (set-face-font 'default "-misc-fixed-medium-r-semicondensed--13-*-*-*-c-60-iso8859-1")


;; The value is in 1/10pt, so 100 gives us 10pt
;; see: http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; Pretty / larger font

;; (when (eq system-type 'gnu/linux)
;;    (set-face-font 'default "Menlo")
;;    (set-face-attribute 'default nil :height 90 :width 'condensed))

(when (eq system-type 'gnu/linux)
  (set-face-font 'default "Monofur")
  (set-face-attribute 'default nil :height 110 :width 'condensed :weight 'light))


;; configuring cedet
;; (require 'cedet)
;; (require 'semantic/sb)
;; (global-ede-mode t)
;; (semantic-mode t)

;; semantic stuff
;; (semantic-add-system-include "/pluto/local/lib/python2.6" 'python-mode)
;; (semantic-add-system-include "/pluto/pluto" 'python-mode)
;; (semantic-add-system-include "/pluto/pycloud" 'python-mode)
