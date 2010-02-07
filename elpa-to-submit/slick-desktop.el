;;; slick-desktop.el --- A slick way to handle saving desktop buffers

;; Copyright (C) 2010 - Mahmoud Abdelkader

;; Author: Mahmoud Abdelkader <mahmoud@linux.com>
;; Version: 0.1
;; Url: http://mahmoudimus.com

;; LICENSE: http://sam.zoy.org/wtfpl/
;;
;;  
;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
;;                    Version 2, December 2004 
;;
;; Copyright (C) 2004 Sam Hocevar 
;;  14 rue de Plaisance, 75014 Paris, France 
;; Everyone is permitted to copy and distribute verbatim or modified 
;; copies of this license document, and changing it is allowed as long 
;; as the name is changed. 
;;
;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE 
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION 
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO. 


;; Commentary:

;; Manages your desktop buffers

;;; TODO:
;;  - handle named desktops
;;  - handle elscreen buffers **
;;  - don't hardcode ~/.emacs.d, use platform independent directories

;;; REFERENCES:
;; ------------------------
;; 1. http://www.emacswiki.org/emacs/desktop-menu.el
;; 2. http://www.emacswiki.org/emacs/DeskTop
;; 3. http://emacs-session.sourceforge.net/
;; 4. http://stackoverflow.com/questions/847962/what-alternate-session-managers-are-available-for-emacs/849180#849180
;; 5. http://scottfrazersblog.blogspot.com/2009/12/emacs-named-desktop-sessions.html


;;; Code:

;;; slick-desktop minor mode

(setq desktop-path '("~/.emacs.d/sessions"))
(setq desktop-dirname "~/.emacs.d/sessions")
(setq desktop-base-file-name "emacs.session")

;; remove desktop after it's been read
(add-hook 'desktop-after-read-hook
      '(lambda ()
         ;; desktop-remove clears desktop-dirname
         (setq desktop-dirname-tmp desktop-dirname)
         (desktop-remove)
         (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
      (desktop-save-in-desktop-dir)
    (message "Session not saved."))
  (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
(add-hook 'after-init-hook
      '(lambda ()
         (if (saved-session)
         (if (y-or-n-p "Restore desktop? ")
             (session-restore)))))


(provide 'slick-desktop)

;;; smart-operator.el ends here
