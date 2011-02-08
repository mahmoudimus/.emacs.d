;; Lucius color scheme for emacs
;;
;; To use add the following to your .emacs file:
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/site-lisp/themes/color-theme-lucius.el")
;; (color-theme-lucius.el)
;;
;; MIT License Copyright (c) 2010 Mahmoud Abdelkader <mahmoud@linux.com>
;; Inspired by the Vim colorscheme Lucius by Jonathan Filip
;;

(defun color-theme-lucius ()
  (interactive)
  (color-theme-install
   '(color-theme-lucius
     ((background-color . "#202020")
      (background-mode . dark)
      (cursor-color . "#5A647E")
      (foreground-color . "#e0e0e0"))
     (default ((t (nil))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (fringe ((t (:background "#232323"))))
     (font-lock-builtin-face ((t (:foreground "#D0D0FF"))))
     (font-lock-comment-face ((t (:foreground "#BC9458" :italic t))))
     (font-lock-constant-face ((t (:foreground "#6D9CBE"))))
     (font-lock-doc-string-face ((t (:foreground "#A5C261"))))
     (font-lock-function-name-face ((t (:foreground "#FFC66D"))))
     (font-lock-keyword-face ((t (:foreground "#CC7833"))))
     (font-lock-preprocessor-face ((t (:foreground "#CC7833"))))
     (font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-string-face ((t (:foreground "#A5C261"))))
     (font-lock-type-face ((t (:foreground "white"))))
     (font-lock-variable-name-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-warning-face ((t (:foreground "Pink"))))
     (paren-face-match-light ((t (:foreground "#FFC66D" :background "#555577"))))
     (highlight ((t (:background "darkolivegreen"))))
     (italic ((t (:italic t))))
     (modeline ((t (:background "#A5BAF1" :foreground "black"))))
     (modeline-buffer-id ((t (:background "#A5BAF1" :foreground 
                                          "black"))))
     (modeline-mousable ((t (:background "#A5BAF1" :foreground 
                                         "black"))))
     (modeline-mousable-minor-mode ((t (:background
                                        "#A5BAF1" :foreground "black"))))
     (region ((t (:background "#555577"))))
     (primary-selection ((t (:background "#555577"))))
     (isearch ((t (:background "#555555"))))
     (zmacs-region ((t (:background "#555577")))) 
     (secondary-selection ((t (:background "darkslateblue")))) 
     (flymake-errline ((t (:background "LightSalmon" :foreground 
                                       "black")))) 
     (flymake-warnline ((t (:background "LightSteelBlue" :foreground 
                                        "black"))))
     (underline ((t (:underline t)))) 
     (minibuffer-prompt ((t (:bold t :foreground "#FF6600")))))))
