;;; -*- mode: lisp -*-
;;; preload-01.el
;;
;; Copyright (c) 2014 -- Mahmoud Abdelkader
;;
;; Author: Mahmoud Abdelkader <mahmoud@linux.com>
;; URL: http://github.com/mahmoudimus/emacs.d/
;; Version: 1.0.0
;; Keywords: convenience, themes

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the preloads for prelude to make
;; the loading process for emacs much faster

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
;; disable prelude-flyspell (it is very slow)
(setq prelude-flyspell nil)

;; flyspell is slow, let's make it fast by ignoring the sit-for
;; see: http://www.brool.com/index.php/speeding-up-flyspell-region
(defadvice flyspell-region (around fast-flyspell-region)
  (cl-flet ( (sit-for (x) t) )
    ad-do-it))
(ad-activate 'flyspell-region)

(provide 'preload-01)

;;; preload-01.el ends here
