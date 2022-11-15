;;; rider-dark-theme.el --- Rider-Dark theme -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Randy Taylor

;; Author: Randy Taylor
;; URL: https://git.sr.ht/~randy/.emacs.d/tree/master/item/lisp/rider-dark-theme.el
;; Version: 0.0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:
;; Rider's default dark theme.

;;; Code:

(deftheme rider-dark)
(let* ((black "#000000")
       (blue "#6394f2")
       (cyan "#3bc6ce")
       (green "#00d08a")
       (magenta "#cb8bff")
       (orange "#d0a164")
       (red "#ff5647")
       (white "#ffffff")
       (yellow "#f5d86a")
       (violet "#fc8ec2")
       (todo "#deadbe")

       (bg "#262626")
       (fg "#bdbdbd")
       (darker-fg "#787878")
       (current-bg "#1f2424")
       (dialog-bg "#292929")

       (comment "#70c760")
       (error "#c65345")
       (success "#479b49")
       (warning "#dfb700")
       (separator "#383838")
       (selection "#345187")
      )

  (custom-theme-set-faces
   'rider-dark
   `(default                                  ((t (:background ,bg :foreground ,fg))))

   `(cursor                                   ((t (:background ,white))))
   `(region                                   ((t (:background ,selection))))
   `(highlight                                ((t (:background ,selection))))
   `(hl-line                                  ((t (:background ,current-bg))))
   `(fringe                                   ((t (:foreground ,separator :background ,bg))))
   `(show-paren-match-face                    ((t (:background ,yellow))))
   `(show-paren-match                         ((t (:foreground ,white :background ,cyan :bold t))))
   `(show-paren-mismatch                      ((t (:background ,bg))))
   `(isearch                                  ((t (:background ,selection))))
   `(lazy-highlight                           ((t (:background ,selection))))
   `(vertical-border                          ((t (:foreground ,separator))))
   `(minibuffer-prompt                        ((t (:foreground ,white :weight normal))))
   `(default-italic                           ((t (:italic t))))
   `(link                                     ((t (:foreground ,blue :underline t))))
   `(error                                    ((t (:foreground ,error))))
   `(yellow                                   ((t (:foreground ,yellow))))
   `(success                                  ((t (:foreground ,success))))
   `(line-number                              ((t (:background ,dialog-bg :foreground ,darker-fg))))
   `(line-number-current-line                 ((t (:background ,dialog-bg :foreground ,darker-fg))))
   `(trailing-whitespace                      ((t (:background ,yellow))))

   `(ffap                                     ((t (:foreground ,white))))

   `(mode-line                                ((t (:foreground ,white :background ,dialog-bg))))
   `(mode-line-inactive                       ((t (:foreground ,white :background ,bg))))
   `(mode-line-buffer-id                      ((t (:foreground ,white))))
   `(mode-line-emphasis                       ((t (:foreground ,white))))

   `(font-lock-preprocessor-face              ((t (:foreground ,cyan))))
   `(font-lock-builtin-face                   ((t (:foreground ,blue))))
   `(font-lock-comment-face                   ((t (:foreground ,comment))))
   `(font-lock-negation-char-face             ((t (:foreground ,magenta))))
   `(font-lock-constant-face                  ((t (:foreground ,cyan))))
   `(font-lock-doc-face                       ((t (:foreground ,red))))
   `(font-lock-function-name-face             ((t (:foreground ,green :bold nil))))
   `(font-lock-keyword-face                   ((t (:foreground ,blue :bold nil))))
   `(font-lock-string-face                    ((t (:foreground ,orange))))
   `(font-lock-type-face                      ((t (:foreground ,magenta))))
   `(font-lock-variable-name-face             ((t (:foreground ,fg))))
   `(font-lock-yellow-face                    ((t (:foreground ,yellow :background ,black))))
   `(font-lock-regexp-grouping-backslash      ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct      ((t (:foreground ,yellow))))
   ;; Custom font-lock faces.
   `(font-lock-escape-face                    ((t (:inherit ,font-lock-keyword-face))))
   `(font-lock-property-face                  ((t (:foreground ,cyan))))
   `(font-lock-number-face                    ((t (:foreground ,violet))))
   `(font-lock-operator-face                  ((t (:foreground ,white :bold t))))

   `(dired-directory                          ((t (:inherit font-lock-keyword-face))))

   ;; corfu.
   `(corfu-annotations                        ((t (:foreground ,orange))))
   `(corfu-border                             ((t (:background ,separator))))
   `(corfu-current                            ((t (:bold t :foreground ,white :background ,selection))))
   `(corfu-default                            ((t (:foreground ,fg :background ,dialog-bg))))
   `(corfu-indexed                            ((t (:foreground ,fg :background ,dialog-bg))))

   `(completions-annotations                  ((t (:bold t :foreground ,green))))
   `(completions-common-part                  ((t (:foreground ,cyan))))

   `(icomplete-selected-match                 ((t (:background ,selection))))

   `(org-document-title                       ((t (:bold t :foreground ,green :height 1.2))))
   `(org-level-1                              ((t (:bold t :foreground ,cyan :height 1.1))))
   `(org-level-2                              ((t (:bold t :foreground ,green))))
   `(org-level-3                              ((t (:bold t :foreground ,magenta))))
   `(org-level-4                              ((t (:bold t :foreground ,red))))
   `(org-level-5                              ((t (:bold t :foreground ,yellow))))
   `(org-code                                 ((t (:inherit link))))
   `(org-hide                                 ((t (:foreground ,white))))
   `(org-date                                 ((t (:underline t :foreground ,white))))
   `(org-footnote                             ((t (:underline t :foreground ,white))))
   `(org-link                                 ((t (:underline t :foreground ,white))))
   `(org-special-keyword                      ((t (:foreground ,yellow))))
   `(org-block                                ((t (:foreground ,white :background ,dialog-bg :extend t))))
   `(org-quote                                ((t (:inherit org-block :slant italic))))
   `(org-verse                                ((t (:inherit org-block :slant italic))))
   `(org-todo                                 ((t (:box (:line-width 1 :color ,red) :foreground ,red :bold nil))))
   `(org-done                                 ((t (:box (:line-width 1 :color ,green) :foreground ,green :bold nil))))
   `(org-yellow                               ((t (:underline t :foreground ,yellow))))
   `(org-scheduled                            ((t (:foreground ,green))))
   `(org-scheduled-today                      ((t (:foreground ,cyan :weight normal :height 1.2))))
   `(org-ellipsis                             ((t (:foreground ,white))))
   `(org-verbatim                             ((t (:foreground ,white))))
   `(org-document-info-keyword                ((t (:foreground ,green))))
   `(org-sexp-date                            ((t (:foreground ,white))))
   `(org-table                                ((t (:foreground ,white :background ,black))))

   ;; bisect
   `(magit-bisect-bad                         ((t (:foreground ,red))))
   `(magit-bisect-good                        ((t (:foreground ,green))))
   `(magit-bisect-skip                        ((t (:foreground ,yellow))))
   ;; blame
   `(magit-blame-date                         ((t (:foreground ,orange))))
   `(magit-blame-heading                      ((t (:foreground ,yellow :background ,black :extend t))))
   ;; branch
   `(magit-branch                             ((t (:foreground ,cyan :weight normal))))
   `(magit-branch-current                     ((t (:foreground ,cyan))))
   `(magit-branch-local                       ((t (:foreground ,cyan))))
   `(magit-branch-remote                      ((t (:foreground ,green))))
   `(magit-cherry-equivalent                  ((t (:foreground ,magenta))))
   `(magit-cherry-unmatched                   ((t (:foreground ,orange))))
   ;; diff
   `(magit-diff-added                         ((t (:foreground ,white :background ,success))))
   `(magit-diff-added-highlight               ((t (:foreground ,white :background ,success))))
   `(magit-diff-removed                       ((t (:foreground ,white :background ,error))))
   `(magit-diff-removed-highlight             ((t (:foreground ,white :background ,error))))
   `(magit-diff-base                          ((t (:foreground ,black :background ,orange :extend t))))
   `(magit-diff-base-highlight                ((t (:foreground ,orange :background ,bg :extend t))))
   `(magit-diff-context                       ((t (:foreground ,white :extend t))))
   `(magit-diff-context-highlight             ((t (:foreground ,white :background ,bg))))
   `(magit-diff-file-header                   ((t (:foreground ,white :background ,bg))))
   `(magit-diff-file-heading                  ((t (:bold t :foreground ,yellow))))
   `(magit-diff-file-heading-highlight        ((t (:background ,bg))))
   `(magit-diff-file-heading-selection        ((t (:background ,bg))))
   `(magit-diff-hunk-heading                  ((t (:foreground ,yellow :background ,bg))))
   `(magit-diff-hunk-heading-highlight        ((t (:background ,bg))))
   `(magit-diff-lines-heading                 ((t (:foreground ,yellow :background ,red :extend t))))
   `(magit-diffstat-added                     ((t (:foreground ,green))))
   `(magit-diffstat-removed                   ((t (:foreground ,red))))
   `(magit-dimmed                             ((t (:foreground ,cyan))))
   `(magit-filename                           ((t (:foreground ,magenta))))
   `(magit-hash                               ((t (:foreground ,cyan))))
   `(magit-header-line                        ((t (:inherit nil))))
   ;; hunk
   `(magit-hunk-heading                       ((t (:background ,yellow))))
   `(magit-hunk-heading-highlight             ((t (:background ,yellow))))
   ;; log
   `(magit-log-author                         ((t (:foreground ,yellow))))
   `(magit-log-date                           ((t (:foreground ,white))))
   `(magit-log-graph                          ((t (:foreground ,cyan))))
   ;; process
   `(magit-mode-line-process                  ((t (:foreground ,orange))))
   `(magit-mode-line-process-error            ((t (:foreground ,red))))
   `(magit-process-ok                         ((t (:inherit success))))
   `(magit-process-ng                         ((t (:inherit error))))
   ;; reflog
   `(magit-reflog-amend                       ((t (:foreground ,magenta))))
   `(magit-reflog-checkout                    ((t (:foreground ,cyan))))
   `(magit-reflog-cherry-pick                 ((t (:foreground ,green))))
   `(magit-reflog-commit                      ((t (:foreground ,green))))
   `(magit-reflog-merge                       ((t (:foreground ,green))))
   `(magit-reflog-other                       ((t (:foreground ,cyan))))
   `(magit-reflog-rebase                      ((t (:foreground ,magenta))))
   `(magit-reflog-remote                      ((t (:foreground ,cyan))))
   `(magit-reflog-reset                       ((t (:inherit error))))
   `(magit-refname                            ((t (:foreground ,cyan))))
   `(magit-section-heading                    ((t (:foreground ,magenta))))
   `(magit-section-heading-selection          ((t (:foreground ,orange :extend t))))
   `(magit-section-highlight                  ((t (:background ,bg :extend t))))
   ;; sequence
   `(magit-sequence-drop                      ((t (:foreground ,red))))
   `(magit-sequence-head                      ((t (:foreground ,cyan))))
   `(magit-sequence-part                      ((t (:foreground ,yellow))))
   `(magit-sequence-stop                      ((t (:foreground ,green))))
   ;; signature
   `(magit-signature-bad                      ((t (:inherit error))))
   `(magit-signature-error                    ((t (:inherit error))))
   `(magit-signature-expired                  ((t (:foreground ,orange))))
   `(magit-signature-good                     ((t (:inherit success))))
   `(magit-signature-revoked                  ((t (:foreground ,magenta))))
   `(magit-signature-untrusted                ((t (:foreground ,yellow))))
   ;; tag
   `(magit-tag                                ((t (:foreground ,orange))))

   ;; shr
   `(shr-text                                 ((t (:inherit nil))))

   ;; gnus
   `(gnus-header-name                         ((t (:foreground ,blue))))
   `(gnus-header-from                         ((t (:foreground ,green))))
   `(gnus-header-name                         ((t (:foreground ,blue))))
   `(gnus-header-subject                      ((t (:foreground ,orange))))
   `(gnus-header-content                      ((t (:foreground ,magenta))))

   ;; egot
   `(eglot-highlight-symbol-face              ((t (:background ,selection :bold t))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rider-dark)

;;; rider-dark-theme.el ends here
