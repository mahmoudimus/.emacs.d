;;; starter-kit-php.el --- Some helpful PHP code
;;; stolen from: https://github.com/yeevgen/emacs-starter-kit/raw/master/starter-kit-php.el
;;; modified by mahmoud abdelkader (mahmoud@linux.com)

;; load the php directory files
;; and set the variable
(setq php-files-dir (concat dotfiles-dir "languages/php/"))

;; load the php subdirectory
(add-to-list 'load-path php-files-dir)

(require 'php-mode)

;; for Zend Views with the .phtml extension
(add-to-list 'auto-mode-alist '("\\.phtml" . php-mode))

(defun flymake-php-enable ()
  (when (and buffer-file-name
             (file-writable-p
              (file-name-directory buffer-file-name))
             (file-writable-p buffer-file-name)
             (if (fboundp 'tramp-list-remote-buffers)
                 (not (subsetp
                       (list (current-buffer))
                       (tramp-list-remote-buffers)))
               t))
    (local-set-key (kbd "C-c d")
                   'flymake-display-err-menu-for-current-line)
    (local-set-key (kbd "C-c n")
                   'flymake-goto-next-error)
    (local-set-key (kbd "C-c p")
                   'flymake-goto-prev-error)
    (flymake-mode t)))

(defun javadoc-php-newline-and-indent ()
  "Advanced C-j for Javadoc multiline comments.
Inserts `*' at the beggining of the new line
unless return was pressed outside the comment"
  (interactive)
  (setq last (point))
  (setq is-inside
    (if (search-backward "*/" nil t)
        ;; there are some comment endings - search forward
        (if (search-forward "/*" last t)
        't
          'nil)

      ;; it's the only comment - search backward
      (goto-char last)
      (if (search-backward "/*" nil t)
          't
        'nil)))

  ;; go to last char position
  (goto-char last)

  ;; the point is inside some comment, insert `*'
  (if is-inside
      (progn
    (newline)
    (insert "* ")
    (indent-according-to-mode))

    ;; else insert only new-line
    (newline-and-indent))
  )

(eval-after-load 'php-mode
  '(progn
     (require 'flymake)
     (add-hook 'php-mode-hook 'flymake-php-enable)
     (add-hook 'php-mode-hook
               (lambda ()
                 (c-set-style "stroustrup")
                 (local-set-key [(control j)] 'javadoc-php-newline-and-indent)))))


(provide 'starter-kit-php)
;; starter-kit-php.el ends here
