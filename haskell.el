;; macbook pro
;; 15"

;; The value is in 1/10pt, so 100 gives us 10pt
;; see: http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; Pretty / larger font
(when (eq system-type 'darwin)
  (set-face-font 'default "FixedSCDZ")
  (set-face-attribute 'default nil :height 130))

;; disable flymake-mode on the mac for now until I can get the script/linters running
(setq flymake-mode nil)

;; on a mac, remove the visibile-bell stuff
(setq visible-bell nil)
