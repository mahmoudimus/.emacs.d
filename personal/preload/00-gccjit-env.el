;;; -*- mode: emacs-lisp; lexical-binding: t;  -*-
;;; 00-gccjit-env.el
;

;; (when (eq system-type 'darwin)
;;   (if (>= emacs-major-version 28)
;;       ;; https://github.com/d12frosted/homebrew-emacs-plus/issues/323
;;       ;; And sure enough the workaround broke with gcc going to version 12, new line:
;;       (setenv "LIBRARY_PATH" "/usr/local/Cellar/gcc/12.2.0/lib/gcc/12:/usr/local/Cellar/libgccjit/12.2.0/lib/gcc/12:/usr/local/Cellar/gcc/12.2.0/lib/gcc/12/gcc/x86_64-apple-darwin20/12")))


(when (and (eq system-type 'darwin)
           (>= emacs-major-version 28))
  ;; Ensure Homebrew is available
  (when (executable-find "brew")
    (let* (
           ;; Helper to trim whitespace from strings
           (trim (lambda (str) (string-trim str)))
           
           ;; Get Homebrew prefixes for gcc and libgccjit
           (gcc-prefix (funcall trim (shell-command-to-string "brew --prefix gcc")))
           (libgccjit-prefix (funcall trim (shell-command-to-string "brew --prefix libgccjit")))
           
           ;; Use 'current' symlink for libgccjit
           (libgccjit-lib-path (concat libgccjit-prefix "/lib/gcc/current/libgccjit"))
           
           ;; Path to lib/gcc/current/gcc/
           (gcc-gcc-dir (concat gcc-prefix "/lib/gcc/current/gcc"))
           
           ;; Function to find the latest aarch64-apple-darwin* directory
           (latest-arch-dir
            (let ((dirs (directory-files gcc-gcc-dir t "^aarch64-apple-darwin[0-9]+$")))
              (when dirs
                (car (sort dirs
                           (lambda (a b)
                             (> (string-to-number (string-match "^.*/aarch64-apple-darwin\\([0-9]+\\)$" a) 
                                                     (match-string 1 a))
                                (string-to-number (string-match "^.*/aarch64-apple-darwin\\([0-9]+\\)$" b)
                                                   (match-string 1 b)))))))))
           
           ;; Find the latest child folder (gcc version) in the latest architecture-specific directory
           (latest-child-folder
            (when latest-arch-dir
              (let ((folders (directory-files latest-arch-dir t "^[0-9]+$")))
                (when folders
                  (car (sort folders
                             (lambda (a b)
                               (> (string-to-number (file-name-nondirectory a))
                                  (string-to-number (file-name-nondirectory b))))))))))
           
           ;; Construct the gcc/gcc/aarch64-apple-darwinXX/YY path
           (gcc-x86-path
            (when (and latest-arch-dir latest-child-folder)
              (concat gcc-prefix "/lib/gcc/current/gcc/"
                      (file-name-nondirectory latest-arch-dir) "/"
                      (file-name-nondirectory latest-child-folder))))
           
           ;; Construct the lib/gcc/current path
           (gcc-lib-path (concat gcc-prefix "/lib/gcc/current")))
      
      ;; Debugging messages (optional, can be removed in production)
      (unless (file-directory-p gcc-gcc-dir)
        (message "gcc-gcc-dir does not exist: %s" gcc-gcc-dir))
      (unless latest-arch-dir
        (message "Failed to find latest architecture-specific directory in %s" gcc-gcc-dir))
      (unless latest-child-folder
        (message "Failed to find child folder in %s" (or latest-arch-dir "nil")))
      
      ;; Only set LIBRARY_PATH if all necessary paths are found
      (if (and gcc-lib-path libgccjit-lib-path gcc-x86-path)
          (progn
            (setenv "LIBRARY_PATH"
                    (mapconcat 'identity
                               (list gcc-lib-path
                                     libgccjit-lib-path
                                     gcc-x86-path)
                               ":"))
            (message "LIBRARY_PATH set to: %s" (getenv "LIBRARY_PATH")))
        (message "LIBRARY_PATH not set due to missing paths.")))))
        
(message "LIBRARY_PATH: %s" (getenv "LIBRARY_PATH"))
(message "PATH: %s" (getenv "PATH"))
(message "exec-path: %s" exec-path)
