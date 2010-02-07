;; macbook pro
;; 15"

;; The value is in 1/10pt, so 100 gives us 10pt
;; see: http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; Pretty / larger font
(when (eq system-type 'darwin)
     (set-face-font 'default "Monaco")
     (set-face-attribute 'default nil :height 110))

