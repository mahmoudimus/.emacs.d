;;; starter-kit-el-get.el -- Allows el-get sources to be defined
;;
;; Part of the Emacs Starter Kit  (added by mahmoudimus)

(require 'cl)

(setq el-get-sources
      '((:name rvm
               :type git
               :url "http://github.com/djwhitt/rvm.el.git"
               :load "rvm.el"
               :compile ("rvm.el")
               :after (lambda() (rvm-use-default)))
        (:name rhtml
               :type git
               :url "https://github.com/eschulte/rhtml.git"
               :features rhtml-mode)
        (:name mmm-mode)
        (:name mmm-mako
               :type hg
               :url "https://bitbucket.org/pjenvey/mmm-mako"
               :compile ("mmm-mako.el"))
        (:name nxhtml-mode)
        ))

(el-get 'sync)

(provide 'starter-kit-el-get)
