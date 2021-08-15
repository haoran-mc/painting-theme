;;; painting-theme.el --- A dark theme  -*- lexical-binding: t; -*-

;; Author: L.M.haoran
;; Keywords: theme
;; Package-Requires: ((emacs "28.0.50"))
;; Version: 1.0.2

;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary: Add (load-file "~/.spacemacs.d/themes/painting-theme.el") into ~/.emacs.d/painting-theme.el

;;; This is a dark theme.

;;; Code:

(deftheme painting "A dark theme.")

(defvar painting-theme-use-italic t
  "Non-nil means use italic for comment and docstring.")

(defvar painting-theme-main-color "#00AAAA"
  "The main color used for some places.
You may want to set this to window's border color.")

(let (
      (italic painting-theme-use-italic)
      (main painting-theme-main-color)
      (white-1         "#E0E0E0")
      (white-2         "#f8f8f2")
      (cyan-1          "#8be9fd")
      (yellow-1        "#CFA300")
      (yellow-2        "#f1fa8c")
      (red             "#E24C49")
      (blue-1          "#009F9F")
      (blue-2          "#0189cc")
      (green-1         "#39BA7E")
      (green-2         "#50fa7b")
      (purple-1        "#B762DE")
      (purple-2        "#B28CE2")
      (purple-3        "#bd93f9")
      (pink-1          "#ff79c6")
      (orange-1        "#FC9F4E")
      (orange-2        "#ffb86c")
      (gray-1          "#AFAFAF")
      (gray-2          "#CCCCCC")
      (gray-3          "#828997")
      (black-1         "#171717")
      (black-2         "#242424")
      (black-3         "#303030")
      (black-4         "#323232")
      (black-5         "#3c3c3c")
      (black-6         "#404040")
      (black-7         "#505050")
      )
  (defconst -test-bg                         black-1)
  (defconst -test-fg                         gray-1)
  (defconst -test-comment-delimiter          gray-1)
  (defconst -test-comment                    blue-1)

  (defvar test-bg                         -test-bg)
  (defvar test-fg                         -test-fg)
  (defvar test-comment-delimiter          -test-comment-delimiter)
  (defvar test-comment                    -test-comment)

  (custom-theme-set-faces
   `painting
   ;; We don't specify default foreground/background in TTY.
   `(default                           ((((type tty))) (((type graphic)) :background ,test-bg :foreground ,test-fg)))
   ;; Font Locks
   `(font-lock-comment-face            ((t (:backaground nil :foreground ,test-comment :italic ,italic))))
   `(font-lock-comment-delimiter-face  ((t (:backaground nil :foreground ,test-comment-delimiter :italic ,italic))))
   `(font-lock-string-face             ((t (:backaground nil :foreground ,orange-1))))
   `(font-lock-doc-face                ((t (:backaground nil :foreground ,blue-1 :italic ,italic))))
   `(font-lock-builtin-face            ((t (:backaground nil :foreground ,red))))
   `(font-lock-type-face               ((t (:backaground nil :foreground ,red))))
   `(font-lock-variable-name-face      ((t (:backaground nil :foreground ,red))))
   `(font-lock-keyword-face            ((t (:backaground nil :foreground ,purple-1))))
   `(font-lock-constant-face           ((t (:backaground nil :foreground ,blue-1))))
   `(font-lock-function-name-face      ((t (:backaground nil :bold t))))
   `(font-lock-warning-face            ((t (:backaground nil :foreground ,red))))
   `(font-lock-preprocessor-face       ((t (:backaground nil :inherit font-lock-constant-face))))

   ;; Basics
   `(cursor                      ((t (:background ,white-1))))
   `(region                      ((t (:background ,black-5))))
   `(hl-line                     ((((type graphic)) :background ,black-2) (((type tty)))))
   `(header-line                 ((t (:background ,black-2 :foreground ,gray-2))))  ;; TODO
   `(fringe                      ((t (:foreground nil :background ,black-6))))
   `(show-paren-match            ((t (:underline ,green-1))))
   `(highlight                   ((t (:background ,black-3))))
   `(button                      ((t (:foreground "#2299CC" :underline t))))
   `(vertical-border             ((t ())))
   `(window-divider              ((t (:foreground ,black-6))))
   `(window-divider-first-pixel  ((t (:foreground ,black-2))))
   `(window-divider-last-pixel   ((t (:foreground ,black-2))))
   `(line-number                 ((t (:foreground ,black-6 :inherit default))))
   `(line-number-current-line    ((((type tty)) :foreground ,yellow-1)
                                  (((type graphic)) :inherit default :foreground ,yellow-1 :background ,black-2)))
   `(completions-common-part     ((t ())))
   `(minibuffer-prompt           ((t ())))
   `(lazy-highlight              ((t (:background ,black-6))))
   `(compilation-info            ((t (:inherit font-lock-function-name-face))))
   `(compilation-warning         ((t (:inherit font-lock-warning-face))))
   `(warning                     ((t (:inherit font-lock-warning-face))))
   `(match                       ((t (:background ,black-3))))

   ;; ISearch
   `(isearch                     ((t (:background ,green-1 :foreground "black"))))
   `(isearch-fail                ((t (:backgronud ,red :foreground "black"))))

   ;; IMenu
   `(imenu-list-entry-face-0          ((t ())))
   `(imenu-list-entry-subalist-face-0 ((t (:bold t))))

   ;; Mode Line
   `(mode-line                        ((t (:background ,black-4))))

   ;; Company
   `(company-tooltip-common           ((t (:bold t))))
   `(company-tooltip-common-selection ((t (:bold t))))
   `(company-tooltip                  ((t (:background ,black-3))))
   `(company-tooltip-selection        ((t (:inverse-video t))))
   `(company-tooltip-annotation       ((t (:foreground ,blue-1))))
   `(company-scrollbar-black-1        ((t (:background ,black-3 :height 0.3))))
   `(company-scrollbar-gray-1         ((t (:background ,black-7 :height 0.3))))
   `(company-template-field           ((t (:inherit yas-field-highlight-face))))

   ;; Ivy
   `(ivy-highlight-face             ((t ())))
   `(ivy-yanked-word                ((t (:background "yellow-1" :foreground "black"))))
   `(ivy-remote                     ((t ())))
   `(ivy-current-match              ((t (:foreground ,black-1 :background ,main))))
   `(ivy-minibuffer-match-highlight ((t ())))
   `(ivy-minibuffer-match-face-1    ((t ())))
   `(ivy-minibuffer-match-face-2    ((t ())))
   `(ivy-minibuffer-match-face-3    ((t ())))
   `(ivy-minibuffer-match-face-4    ((t ())))
   `(counsel-outline-default        ((t ())))
   `(swiper-background-match-face-1 ((t (:inherit hl-line))))
   `(swiper-background-match-face-2 ((t (:inherit hl-line))))
   `(swiper-background-match-face-3 ((t (:inherit hl-line))))
   `(swiper-background-match-face-4 ((t (:inherit hl-line))))
   `(swiper-match-face-1            ((t (:foreground "white-1"))))
   `(swiper-match-face-2            ((t (:foreground "white-1"))))
   `(swiper-match-face-3            ((t (:foreground "white-1"))))
   `(swiper-match-face-4            ((t (:foreground "white-1"))))

   ;; SMerge
   `(smerge-refined-added           ((t (:background "#253325"))))
   `(smerge-lower                   ((t (:background "#173017"))))

   ;; Diff-hl
   `(diff-hl-insert                 ((t (:foreground ,green-1 :background ,green-1))))
   `(diff-hl-change                 ((t (:foreground ,blue-1 :background ,blue-1))))
   `(diff-hl-delete                 ((t (:foreground ,red :background ,red))))

   `(tooltip                        ((t ())))
   `(dired-directory                ((t (:foreground ,purple-2))))
   `(web-mode-html-tag-face         ((t (:foreground ,green-1))))
   `(web-mode-html-attr-name-face   ((t (:foreground ,red))))

   ;; Web Mode
   `(web-mode-function-call-face    ((t ())))
   `(web-mode-function-name-face    ((t ())))
   `(web-mode-html-tag-bracket-face ((t (:inherit parenthesis))))
   `(web-mode-symbol-face           ((t (:foreground ,purple-1))))
   `(css-selector                   ((t (:foreground ,purple-1))))

   ;; Markdown
   `(markdown-header-face-1         ((t (:inherit outline-1 :height 1.0 :foreground "#FD971F"))))
   `(markdown-header-face-2         ((t (:inherit outline-2 :height 1.0 :foreground "#A6E22E"))))
   `(markdown-header-face-3         ((t (:inherit outline-3 :height 1.0 :foreground "#66D9EF"))))
   `(markdown-header-face-4         ((t (:inherit outline-4 :height 1.0 :foreground "#E6DB74"))))
   `(markdown-header-face-5         ((t (:inherit outline-5 :height 1.0 :foreground "#A1EFE4"))))
   `(markdown-header-face-6         ((t (:inherit outline-6 :height 1.0 :foreground "#A6E22E"))))
   `(markdown-header-face-7         ((t (:inherit outline-7 :height 1.0 :foreground "#F92672"))))
   `(markdown-header-face-8         ((t (:inherit outline-8 :height 1.0 :foreground "#66D9EF"))))

   ;; Org-mode
   `(org-table                      ((t (:foreground ,gray-2))))
   `(org-level-1                    ((t (:inherit outline-1 :height 1.0 :weight normal :foreground ,pink-1))))
   `(org-level-2                    ((t (:inherit outline-2 :height 1.0 :weight normal :foreground ,purple-3))))
   `(org-level-3                    ((t (:inherit outline-3 :height 1.0 :weight normal :foreground ,green-2))))
   `(org-level-4                    ((t (:inherit outline-4 :height 1.0 :weight normal :foreground ,yellow-2))))
   `(org-level-5                    ((t (:inherit outline-5 :height 1.0 :weight normal :foreground ,cyan-1))))
   `(org-level-6                    ((t (:inherit outline-6 :height 1.0 :weight normal :foreground ,orange-2))))
   `(org-level-7                    ((t (:inherit outline-7 :height 1.0 :weight normal :foreground ,blue-2))))
   `(org-level-8                    ((t (:inherit outline-8 :height 1.0 :weight normal :foreground ,white-2))))
   `(org-document-title             ((t (:inherit font-lock-string-face))))
   `(org-code                       ((t (:inherit font-lock-constant-face))))

   ;; Treemacs
   `(treemacs-root-face             ((t (:inherit font-lock-function-name-face :height 1.4 :underline t)))))
  )

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'painting)
;;; painting-theme.el ends here
