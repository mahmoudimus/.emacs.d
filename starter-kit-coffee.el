;;; starter-kit-coffee.el --- Some helpful coffee script code
;;; by mahmoud abdelkader (mahmoud@linux.com)

;; load the coffee script directory files
;; and set the variable
(setq coffee-files-dir (concat dotfiles-dir "languages/coffee/"))

;; load the php subdirectory
(add-to-list 'load-path coffee-files-dir)

(require 'coffee-mode)

;; for more ..
;; (add-to-list 'auto-mode-alist '("\\.coffee" . coffee-mode))

(provide 'starter-kit-coffee)
