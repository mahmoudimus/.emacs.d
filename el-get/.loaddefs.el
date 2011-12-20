;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rvm-open-gem rvm-use rvm-activate-corresponding-ruby
;;;;;;  rvm-use-default) "rvm" "rvm/rvm.el" (20028 30790))
;;; Generated autoloads from rvm/rvm.el

(autoload 'rvm-use-default "rvm" "\
use the rvm-default ruby as the current ruby version

\(fn)" t nil)

(autoload 'rvm-activate-corresponding-ruby "rvm" "\
activate the corresponding ruby version for the file in the current buffer.
This function searches for an .rvmrc file and actiavtes the configured ruby.
If no .rvmrc file is found, the default ruby is used insted.

\(fn)" t nil)

(autoload 'rvm-use "rvm" "\
switch the current ruby version to any ruby, which is installed with rvm

\(fn NEW-RUBY NEW-GEMSET)" t nil)

(autoload 'rvm-open-gem "rvm" "\
Not documented

\(fn GEMHOME)" t nil)

;;;***

;;;### (autoloads nil nil ("mmm-mako/mmm-mako.el" "mmm-mode/mmm-auto.el"
;;;;;;  "mmm-mode/mmm-class.el" "mmm-mode/mmm-cmds.el" "mmm-mode/mmm-compat.el"
;;;;;;  "mmm-mode/mmm-cweb.el" "mmm-mode/mmm-mason.el" "mmm-mode/mmm-mode.el"
;;;;;;  "mmm-mode/mmm-myghty.el" "mmm-mode/mmm-noweb.el" "mmm-mode/mmm-region.el"
;;;;;;  "mmm-mode/mmm-rpm.el" "mmm-mode/mmm-sample.el" "mmm-mode/mmm-univ.el"
;;;;;;  "mmm-mode/mmm-utils.el" "mmm-mode/mmm-vars.el") (20034 63197
;;;;;;  835991))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
