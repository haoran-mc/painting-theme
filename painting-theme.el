;;; painting-theme.el --- A minimal dark theme  -*- lexical-binding: t; -*-

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

;;; Commentary:
;; Add (load-file "~/.spacemacs.d/themes/painting-theme.el") into ~/.emacs.d/painting-theme.el

;;; This is a concise and dark theme.

;;; Code:

(deftheme painting "A dark theme.")

(let (
      (italic t)
      (main "#8b8b8b")
      (black-1         "#171717")
      (black-2         "#242424")
      (black-3         "#303030")
      (black-4         "#323232")
      (black-6         "#404040")
      (ash           "#7c7c7c")
	  (gray          "#AFAFAF")

      (pink          "#ff79c6")
      (orange        "#FC9F4E")
      (red           "#E24C49")
      (white         "#E0E0E0")
      (yellow        "#CFA300")
      (purple        "#B762DE")
      (cyan          "#8BE9FD")
      (green         "#39BA7E")

      (lightblue          "#96cbfe")
      (lightgreen         "#50FA7B")
      (lightorange        "#ffb86c")
      (lightpurple        "#BD93F9")

      (deepcyan          "#009F9F")
      )
  (defconst -test-fg                         gray)
  (defconst -test-bg                         black-1)
  (defconst -test-comment                    ash)
  (defconst -test-comment-delimiter          ash)

  (defvar test-fg                         -test-fg)
  (defvar test-bg                         -test-bg)
  (defvar test-comment                    -test-comment)
  (defvar test-comment-delimiter          -test-comment-delimiter)

  (custom-theme-set-faces
   `painting

   `(default ((:background ,test-bg :foreground ,test-fg))) ;;


   `(font-lock-comment-face            ((t (:foreground ,test-comment :italic ,italic)))) ;;
   `(font-lock-comment-delimiter-face  ((t (:foreground ,test-comment-delimiter :italic ,italic)))) ;;
   `(font-lock-string-face             ((t (:foreground ,orange)))) ;;
   `(font-lock-doc-face                ((t (:foreground ,deepcyan :italic ,italic)))) ;;
   `(font-lock-builtin-face            ((t (:foreground ,test-fg)))) ;;
   `(font-lock-type-face               ((t (:foreground ,test-fg)))) ;;
   `(font-lock-variable-name-face      ((t (:foreground ,test-fg)))) ;;
   `(font-lock-keyword-face            ((t (:foreground ,lightpurple)))) ;;
   `(font-lock-constant-face           ((t (:foreground ,deepcyan)))) ;;
   `(font-lock-function-name-face      ((t (:foreground ,gray :bold nil)))) ;;
   `(font-lock-warning-face            ((t (:foreground ,red)))) ;;
   `(font-lock-preprocessor-face       ((t (:inherit font-lock-constant-face)))) ;;

   ;; Basics
   `(fringe                      ((t (:foreground nil :background nil))))
   `(vertical-border             ((t (:foreground ,black-1))))
   `(cursor                      ((t (:background ,white))))
   `(region                      ((t (:background ,black-4))))
   `(hl-line                     ((((type graphic)) :background ,black-2) (((type tty)))))
   `(header-line                 ((t (:background ,black-1 :foreground ,white))))
   `(show-paren-match            ((t (:underline ,green))))
   `(highlight                   ((t (:background ,black-3))))
   `(button                      ((t (:foreground "#2299CC" :underline t))))
   `(vertical-border             ((t ())))
   `(window-divider              ((t (:foreground ,black-6))))
   `(window-divider-first-pixel  ((t (:foreground ,black-2))))
   `(window-divider-last-pixel   ((t (:foreground ,black-2))))
   `(line-number                 ((t (:foreground ,black-6 :inherit default))))
   `(line-number-current-line    ((((type tty)) :foreground ,yellow)
                                  (((type graphic)) :inherit default :foreground ,yellow :background ,black-2)))
   `(completions-common-part     ((t ())))
   `(minibuffer-prompt           ((t ())))
   '(lazy-highlight              ((t (:background "#86dc2f" :foreground "#262626"))))
   `(compilation-info            ((t (:inherit font-lock-function-name-face))))
   `(compilation-warning         ((t (:inherit font-lock-warning-face))))
   `(warning                     ((t (:inherit font-lock-warning-face))))
   `(match                       ((t (:background ,black-3))))
   `(which-func                  ((t (:foreground ,gray :bold nil))))

   ;; ISearch
   `(isearch                     ((t (:background ,green :foreground "black"))))
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
   `(company-tooltip-annotation       ((t (:foreground ,deepcyan))))
   `(company-scrollbar-black-1        ((t (:background ,black-3 :height 0.3))))
   `(company-scrollbar-gray         ((t (:background ,black-6 :height 0.3))))
   `(company-template-field           ((t (:inherit yas-field-highlight-face))))

   ;; Ivy
   `(ivy-highlight-face             ((t ())))
   `(ivy-yanked-word                ((t (:background "yellow" :foreground "black"))))
   `(ivy-remote                     ((t ())))
   `(ivy-minibuffer-match-highlight ((t (:background ,main :foreground ,black-1 :weight bold))))
   `(ivy-current-match              ((t (:background ,main :foreground ,black-1 :weight bold))))
   `(ivy-minibuffer-match-face-1    ((t ())))
   `(ivy-minibuffer-match-face-2    ((t (:background ,main :foreground ,black-1 :weight bold))))
   `(ivy-minibuffer-match-face-3    ((t ())))
   `(ivy-minibuffer-match-face-4    ((t ())))
   `(counsel-outline-default        ((t ())))
   `(swiper-background-match-face-1 ((t (:inherit hl-line))))
   `(swiper-background-match-face-2 ((t (:inherit hl-line))))
   `(swiper-background-match-face-3 ((t (:inherit hl-line))))
   `(swiper-background-match-face-4 ((t (:inherit hl-line))))
   `(swiper-match-face-1            ((t (:foreground "white"))))
   `(swiper-match-face-2            ((t (:foreground "white"))))
   `(swiper-match-face-3            ((t (:foreground "white"))))
   `(swiper-match-face-4            ((t (:foreground "white"))))

   ;; Diff-hl
   `(diff-hl-insert                 ((t (:foreground ,green :background ,green))))
   `(diff-hl-change                 ((t (:foreground ,deepcyan :background ,deepcyan))))
   `(diff-hl-delete                 ((t (:foreground ,red :background ,red))))

   `(tooltip                        ((t ())))
   `(dired-directory                ((t (:foreground ,lightblue))))

   ;; Web Mode
   `(web-mode-function-call-face    ((t ())))
   `(web-mode-function-name-face    ((t ())))
   `(web-mode-symbol-face           ((t (:foreground ,purple))))
   `(css-selector                   ((t (:foreground ,purple))))

   ;; Markdown
   `(markdown-header-face-1         ((t (:inherit outline-1 :height 1.0 :foreground "#FD971F"))))
   `(markdown-header-face-2         ((t (:inherit outline-2 :height 1.0 :foreground "#A6E22E"))))
   `(markdown-header-face-3         ((t (:inherit outline-3 :height 1.0 :foreground "#66D9EF"))))
   `(markdown-header-face-4         ((t (:inherit outline-4 :height 1.0 :foreground "#E6DB74"))))
   `(markdown-header-face-5         ((t (:inherit outline-5 :height 1.0 :foreground "#A1EFE4"))))
   `(markdown-header-face-6         ((t (:inherit outline-6 :height 1.0 :foreground "#A6E22E"))))
   `(markdown-header-face-7         ((t (:inherit outline-7 :height 1.0 :foreground "#F92672"))))
   `(markdown-header-face-8         ((t (:inherit outline-8 :height 1.0 :foreground "#66D9EF"))))

   ;; mode-line
   `(mode-line ((t (:background ,black-3))))
   `(mode-line-inactive ((t (:inverse-video nil (list
												 :foreground ,ash
												 :background ,black-1
												 :box ,black-1)))))

   ;; Org-mode
   `(shadow            ((t (:foreground ,test-comment))))
   `(org-level-1       ((t (:inherit outline-1 :height 1.0 :weight normal :foreground ,pink))))
   `(org-level-2       ((t (:inherit outline-2 :height 1.0 :weight normal :foreground ,lightpurple))))
   `(org-level-3       ((t (:inherit outline-3 :height 1.0 :weight normal :foreground ,lightgreen))))
   `(org-level-4       ((t (:inherit outline-4 :height 1.0 :weight normal :foreground ,yellow))))
   `(org-level-5       ((t (:inherit outline-5 :height 1.0 :weight normal :foreground ,cyan))))
   `(org-level-6       ((t (:inherit outline-6 :height 1.0 :weight normal :foreground ,lightorange))))
   `(org-level-7       ((t (:inherit outline-7 :height 1.0 :weight normal :foreground ,lightblue))))
   `(org-level-8       ((t (:inherit outline-8 :height 1.0 :weight normal :foreground ,white))))
   `(org-document-title             ((t (:inherit font-lock-string-face))))
   `(org-code                       ((t (:inherit font-lock-constant-face))))
   `(org-link                       ((t (:foreground ,blue-5 :underline t))))
   `(org-ellipsis                   ((t (:foreground ,test-comment))))
   `(org-table                      ((t (:foreground ,lightpurple))))
   `(org-document-info              ((t (:foreground "#0189cc"))))
   `(org-document-info-keyword      ((t (:foreground "#6272a4"))))
   `(org-document-title             ((t (:weight bold :foreground "#ffb86c"))))
   `(org-date                       ((t (:foreground ,blue-5))))

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
