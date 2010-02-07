(setenv "PYTHONPATH" "/pluto/pluto:/pluto/pycloud")
(setenv "LD_LIBRARY_PATH" "/pluto/local/lib:/usr/local/lib")
(setenv "PATH" "/home/mahmoud/bin:/pluto/local/bin:/home/mahmoud/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games")

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

;; The value is in 1/10pt, so 100 gives us 10pt
;; see: http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; Pretty / larger font
(when (eq system-type 'gnu/linux)
    (set-face-font 'default "Menlo")
    (set-face-attribute 'default nil :height 90))
