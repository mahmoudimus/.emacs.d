(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("ELPA" . "http://tromey.com/elpa/") t)
(package-initialize)
