;;; monokai-theme.el --- A fruity color theme for Emacs.

;; Copyright (C) 2011-2016

;; Author: Kelvin Smith <oneKelvinSmith@gmail.com>
;; URL: http://github.com/oneKelvinSmith/monokai-emacs
;; Version: 3.5.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Textmate theme Monokai for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Code:

(deftheme monokai "The Monokai colour theme")

(defgroup monokai nil
  "Monokai theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom monokai-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'monokai)

(defcustom monokai-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'monokai)

(defcustom monokai-doc-face-as-comment nil
  "Consider `font-lock-doc-face' as comment instead of a string."
  :type 'boolean
  :group 'monokai
  :package-version "3.5.1")

(defcustom monokai-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'monokai)

(defcustom monokai-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'monokai)

;; Primary colors
(defcustom monokai-yellow "#E6DB74"
  "Primary colors - yellow"
  :type 'string
  :group 'monokai)

(defcustom monokai-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'monokai)

(defcustom monokai-red "#F92672"
  "Primary colors - red"
  :type 'string
  :group 'monokai)

(defcustom monokai-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'monokai)

(defcustom monokai-blue "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'monokai)

(defcustom monokai-green "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'monokai)

(defcustom monokai-cyan "#009F9F"
  "Primary colors - cyan"
  :type 'string
  :group 'monokai)

(defcustom monokai-violet "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'monokai)

(defcustom monokai-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'monokai)

(defcustom monokai-foreground "#D6D6D4"
  "Adaptive colors - foreground"
  :type 'string
  :group 'monokai)

(defcustom monokai-background "#1C1E1F"
  "Adaptive colors - background"
  :type 'string
  :group 'monokai)

(defcustom monokai-comments "#525254"
  "Adaptive colors - comments"
  :type 'string
  :group 'monokai)

(defcustom monokai-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'monokai)

(defcustom monokai-line-number "#8F908A"
  "Adaptive colors - line number"
  :type 'string
  :group 'monokai)

(defcustom monokai-highlight "#49483E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'monokai)

(defcustom monokai-highlight-alt "#2D2E2E"
  "Adaptive colors - highlight"
  :type 'string
  :group 'monokai)

(defcustom monokai-highlight-line "#222323"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'monokai)

(let* (;; Variable pitch
       (monokai-pitch (if monokai-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (monokai-class '((class color) (min-colors 257)))

       ;; Functionality specific colors
       (monokai-diff-blue-base      "#232438")
       (monokai-diff-blue-emphasis  "#1F204E")
       (monokai-diff-green-base     "#233E1E")
       (monokai-diff-green-emphasis "#1F541A")
       (monokai-diff-red-base       "#3D241E")
       (monokai-diff-red-emphasis   "#53201A")

       ;; Darker and lighter accented colors
       (monokai-yellow-d       "#BEB244")
       (monokai-yellow-l       "#FFF7A8")
       (monokai-orange-d       "#D47402")
       (monokai-orange-l       "#FFAC4A")
       (monokai-red-d          "#F70057")
       (monokai-red-l          "#FA518D")
       (monokai-magenta-d      "#FB35EA")
       (monokai-magenta-l      "#FE8CF4")
       (monokai-violet-d       "#945AFF")
       (monokai-violet-l       "#C9ACFF")
       (monokai-blue-d         "#40CAE4")
       (monokai-blue-l         "#92E7F7")
       (monokai-cyan-l         "#A1EFE4") ;; 74DBCD
       (monokai-green-d        "#86C30D")
       (monokai-green-l        "#BBEF53")
       (monokai-gray-d         "#35331D")
       (monokai-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (monokai-foreground-hc  "#141414")
       (monokai-foreground-lc  "#171A0B")
       ;; High contrast colors
       (monokai-yellow-hc      "#FFFACE")
       (monokai-yellow-hc-alt  "#E7DB74")
       (monokai-yellow-lc      "#9A8F21")
       (monokai-orange-hc      "#FFBE74")
       (monokai-orange-lc      "#A75B00")
       (monokai-red-hc         "#FEB0CC")
       (monokai-red-hc-alt     "#F83535")
       (monokai-red-lc         "#F20055")
       (monokai-magenta-hc     "#FEC6F9")
       (monokai-magenta-lc     "#F309DF")
       (monokai-violet-hc      "#F0E7FF")
       (monokai-violet-lc      "#7830FC")
       (monokai-blue-hc        "#CAF5FD")
       (monokai-blue-lc        "#1DB4D0")
       (monokai-cyan-hc        "#D3FBF6")
       (monokai-cyan-lc        "#4BBEAE")
       (monokai-green-hc       "#CCF47C")
       (monokai-green-hc-alt   "#A6E22C")
       (monokai-green-lc       "#679A01")

       ;; Distinct fringe
       (monokai-fringe-bg (if monokai-distinct-fringe-background
                              monokai-gray
                            monokai-background))

       ;; Definitions for terminals that do not support 256 colors
       (monokai-256-class '((class color) (min-colors 89)))

       ;; 256 Primary colors
       (monokai-256-yellow         "#CDC673")
       (monokai-256-orange         "#FF8C00")
       (monokai-256-red            "#FF1493")
       (monokai-256-blue           "#5FD7FF")
       (monokai-256-green          "#87D700")
       (monokai-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (monokai-256-yellow-d       "#878700")
       (monokai-256-yellow-l       "#FFFF87")
       (monokai-256-red-d          "#870000")
       (monokai-256-red-l          "#FF5F87")
       (monokai-256-green-d        "#5F8700")
       (monokai-256-green-l        "#AFD700")
       ;; Adaptive colors
       (monokai-256-background     "#1B1E1C")
       ;; High contrast colors
       (monokai-256-yellow-hc      monokai-256-yellow-d)
       (monokai-256-yellow-lc      monokai-256-yellow-l)
       (monokai-256-red-hc         monokai-256-red-d)
       (monokai-256-red-lc         monokai-256-red-l)
       (monokai-256-green-hc       monokai-256-green-d)
       (monokai-256-green-lc       monokai-256-green-l)

       ;; Distinct fringe
       (monokai-256-fringe-bg (if monokai-distinct-fringe-background
                                  monokai-256-gray
                                monokai-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'monokai

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face           ((t (:foreground ,monokai-red :weight normal))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,monokai-comments))))
   `(font-lock-comment-face           ((t (:foreground ,monokai-comments))))
   `(font-lock-constant-face          ((t (:foreground ,monokai-violet))))
   `(font-lock-doc-face               ((t (:foreground ,(if monokai-doc-face-as-comment
                                                            monokai-comments
                                                          monokai-cyan)))))

   `(font-lock-function-name-face ((t (:foreground ,monokai-green))))
   `(font-lock-keyword-face       ((t (:foreground ,monokai-red :weight normal))))
   `(font-lock-negation-char-face ((t (:foreground ,monokai-cyan))))
   `(font-lock-preprocessor-face  ((t (:foreground ,monokai-red))))
   `(font-lock-string-face        ((t (:foreground ,monokai-cyan))))
   `(font-lock-type-face          ((t (:foreground ,monokai-blue :italic nil))))
   `(font-lock-variable-name-face ((t (:foreground ,monokai-orange))))

   `(font-lock-warning-face ((t (:foreground ,monokai-orange
                                             :weight bold
                                             :italic t
                                             :underline t))))

   `(font-lock-regexp-grouping-construct ((t (:foreground ,monokai-cyan :weight normal))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,monokai-violet :weight normal))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button         ((t (:underline t))))
   `(default        ((t (:foreground ,monokai-foreground :background ,monokai-background))))
   `(highlight      ((t (:background ,monokai-highlight))))
   `(lazy-highlight ((t (:inherit highlight :background ,monokai-highlight-alt))))
   `(region         ((t (:inherit highlight :background ,monokai-highlight))))
   `(shadow         ((t (:foreground ,monokai-comments))))
   `(match          ((t (:background ,monokai-green :foreground ,monokai-background))))
   `(fringe         ((t (:foreground ,monokai-foreground :background ,monokai-fringe-bg))))
   `(success        ((t (:foreground ,monokai-green))))
   `(warning        ((t (:foreground ,monokai-yellow))))
   `(error          ((t (:foreground ,monokai-red))))
   `(link           ((t (:foreground ,monokai-blue :underline t))))
   `(link-visited   ((t (:foreground ,monokai-violet
                                     :underline t
                                     :weight normal))))

   `(secondary-selection ((t (:inherit region :background ,monokai-highlight-alt))))
   `(cursor ((t (:foreground ,monokai-background
                             :background ,monokai-foreground
                             :inverse-video t))))

   `(mouse ((t (:foreground ,monokai-background
                            :background ,monokai-foreground
                            :inverse-video t))))

   `(escape-glyph ((t (:foreground ,monokai-comments))))
   `(escape-glyph-face ((t (:foreground ,monokai-comments))))

   `(eval-sexp-fu-flash ((t (:foreground ,monokai-background
                                         :background ,monokai-green))))

   `(eval-sexp-fu-flash-error ((t (:foreground ,monokai-background
                                               :background ,monokai-red))))

   `(trailing-whitespace ((t (:background ,monokai-red))))
   `(vertical-border ((t (:foreground ,monokai-gray))))
   `(menu ((t (:foreground ,monokai-foreground :background ,monokai-background))))
   `(minibuffer-prompt ((t (:foreground ,monokai-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,monokai-green))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,monokai-emphasis
                                   :background ,monokai-highlight
                                   :box (:line-width 1
                                                     :color ,monokai-gray
                                                     :style nil)))))

   `(powerline-active1 ((t (:background ,monokai-gray-d))))
   `(powerline-active2 ((t (:background ,monokai-background))))
   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,monokai-comments
                                            :background ,monokai-background
                                            :box (:line-width 1
                                                              :color ,monokai-gray
                                                              :style nil)))))

   `(powerline-inactive1 ((t (:background ,monokai-gray-d))))
   `(powerline-inactive2 ((t (:background ,monokai-background))))

   ;; header-line
   `(header-line ((t (:foreground ,monokai-emphasis
                                  :background ,monokai-highlight
                                  :box (:color ,monokai-gray
                                               :line-width 1
                                               :style nil)))))

   ;; tab-line
   `(tab-line ((t (:foreground ,monokai-foreground :background ,monokai-highlight))))
   `(tab-line-highlight ((t (:underline t))))
   `(tab-line-tab ((t (:foreground ,monokai-foreground
				                   :background ,monokai-background
				                   :box (:line-width 4 :color ,monokai-background)))))
   `(tab-line-tab-current ((t (:inherit tab-line-tab))))
   `(tab-line-tab-inactive ((t (:inherit tab-line-tab
				                         :foreground ,monokai-comments
				                         :background ,monokai-highlight
				                         :box (:line-width 4 :color ,monokai-highlight)))))
   `(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive))))
   `(tab-line-tab-modified ((t (:inherit tab-line-tab))))

   ;; tab-bar
   `(tab-bar ((t (:background ,monokai-background))))
   `(tab-bar-tab ((t (:foreground ,monokai-background
                                  :background ,monokai-orange
                                  :box (:line-width -2 :color ,monokai-orange)))))
   `(tab-bar-tab-inactive ((t (:foreground ,monokai-foreground
                                           :background ,monokai-background))))

   ;; hl-todo
   `(hl-todo ((t (:weight normal))))

   ;; window-tool-bar
   `(window-tool-bar-button ((t (:inherit tab-line))))
   `(window-tool-bar-button-hover ((t (:inherit tab-line :inverse-video t))))
   `(window-tool-bar-button-disabled ((t (:inherit shadow :background ,monokai-highlight-alt))))

   ;; cua
   `(cua-global-mark ((t (:background ,monokai-yellow :foreground ,monokai-background))))
   `(cua-rectangle ((t (:inherit region))))
   `(cua-rectangle-noselect ((t (:inherit secondary-selection))))

   ;; diary
   `(diary ((t (:foreground ,monokai-yellow))))

   ;; dired
   `(dired-directory ((t (:foreground ,monokai-blue))))
   `(dired-flagged ((t (:foreground ,monokai-red))))
   `(dired-header ((t (:foreground ,monokai-blue :background ,monokai-highlight))))
   `(dired-ignored ((t (:inherit shadow))))
   `(dired-mark ((t (:foreground ,monokai-green))))
   `(dired-marked ((t (:foreground ,monokai-violet :inherit bold))))
   `(dired-perm-write ((t (:foreground ,monokai-foreground :underline t))))
   `(dired-symlink ((t (:foreground ,monokai-cyan-l :slant italic))))
   `(dired-warning ((t (:foreground ,monokai-orange :underline t))))

   ;; dropdown
   `(dropdown-list-face ((t (:background ,monokai-highlight-line :foreground ,monokai-blue))))
   `(dropdown-list-selection-face ((t (:background ,monokai-green
                                                   :foreground ,monokai-background))))

   ;; grep
   `(grep-context-face ((t (:foreground ,monokai-foreground))))
   `(grep-error-face ((t (:foreground ,monokai-red :underline t))))
   `(grep-hit-face ((t (:foreground ,monokai-orange))))
   `(grep-match-face ((t (:foreground ,monokai-green))))

   ;; isearch
   `(isearch ((t (:inherit region
                           :foreground ,monokai-background
                           :background ,monokai-yellow))))

   `(isearch-fail ((t (:inherit isearch
                                :foreground ,monokai-red
                                :background ,monokai-background
                                :bold t))))

   ;; ace-jump-mode
   `(ace-jump-face-background ((t (:foreground ,monokai-comments
                                               :background ,monokai-background
                                               :inverse-video nil))))

   `(ace-jump-face-foreground ((t (:foreground ,monokai-yellow
                                               :background ,monokai-background
                                               :inverse-video nil))))

   ;; anzu-mode
   `(anzu-mode-line ((t (:foreground ,monokai-violet))))

   ;; bm
   `(bm-face ((t (:background ,monokai-yellow-lc :foreground ,monokai-background))))
   `(bm-fringe-face ((t (:background ,monokai-yellow-lc :foreground ,monokai-background))))
   `(bm-persistent-face ((t (:background ,monokai-green-lc :foreground ,monokai-background))))
   `(bm-fringe-persistent-face ((t (:background ,monokai-green-lc
                                                :foreground ,monokai-background))))

   ;; cider
   `(cider-enlightened ((t (:foreground ,monokai-yellow
                                        :background unspecified
                                        :box (:color ,monokai-yellow :line-width -1 :style nil)))))

   `(cider-enlightened-local ((t (:foreground ,monokai-yellow))))
   `(cider-instrumented-face ((t (:foreground ,monokai-violet
                                              :background unspecified
                                              :box (:color ,monokai-violet :line-width -1 :style nil)))))
   `(cider-result-overlay-face ((t (:foreground ,monokai-blue
                                                :background unspecified
                                                :box (:color ,monokai-blue :line-width -1 :style nil)))))
   `(cider-test-error-face ((t (:foreground ,monokai-background
                                            :background ,monokai-orange))))
   `(cider-test-failure-face ((t (:foreground ,monokai-background
                                              :background ,monokai-red))))
   `(cider-test-success-face ((t (:foreground ,monokai-background
                                              :background ,monokai-green))))

   `(cider-traced-face ((t :box (:color ,monokai-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face ((t (:foreground ,monokai-red :underline t))))
   `(clojure-test-error-face   ((t (:foreground ,monokai-orange :underline t))))
   `(clojure-test-success-face ((t (:foreground ,monokai-green :underline t))))

   ;; company-mode
   `(company-tooltip ((t (:background ,monokai-highlight-line :foreground ,monokai-emphasis))))
   `(company-tooltip-selection ((t (:background ,monokai-blue :foreground ,monokai-background))))
   `(company-tooltip-mouse ((t (:background ,monokai-blue :foreground ,monokai-background))))
   `(company-tooltip-common ((t (:foreground ,monokai-blue :underline t))))
   `(company-tooltip-common-selection ((t (:foreground ,monokai-background
                                                       :background ,monokai-blue
                                                       :underline t))))

   `(company-preview ((t (:background ,monokai-highlight-line :foreground ,monokai-emphasis))))
   `(company-preview-common ((t (:foreground ,monokai-blue :underline t))))
   `(company-scrollbar-bg ((t (:background ,monokai-gray))))
   `(company-scrollbar-fg ((t (:background ,monokai-comments))))
   `(company-tooltip-annotation ((t (:background ,monokai-highlight-line
                                                 :foreground ,monokai-green))))

   `(company-template-field ((t (:background ,monokai-highlight-line
                                             :foreground ,monokai-blue))))

   ;; compilation
   `(compilation-column-face ((t (:foreground ,monokai-cyan-l :underline nil))))
   `(compilation-column-number ((t (:inherit font-lock-doc-face
                                             :foreground ,monokai-cyan-l
                                             :underline nil))))

   `(compilation-enter-directory-face ((t (:foreground ,monokai-green :underline nil))))
   `(compilation-error ((t (:inherit error :underline nil))))
   `(compilation-error-face ((t (:foreground ,monokai-red :underline nil))))
   `(compilation-face ((t (:foreground ,monokai-foreground :underline nil))))
   `(compilation-info ((t (:foreground ,monokai-comments
                                       :underline nil
                                       :bold nil))))

   `(compilation-info-face ((t (:foreground ,monokai-blue :underline nil))))
   `(compilation-leave-directory-face ((t (:foreground ,monokai-green :underline nil))))
   `(compilation-line-face ((t (:foreground ,monokai-green :underline nil))))
   `(compilation-line-number ((t (:foreground ,monokai-green :underline nil))))
   `(compilation-warning ((t (:inherit warning :underline nil))))
   `(compilation-warning-face ((t (:foreground ,monokai-yellow
                                               :weight normal
                                               :underline nil))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info
                                              :foreground ,monokai-green))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error
                                              :foreground ,monokai-red))))

   `(compilation-mode-line-run ((t (:foreground ,monokai-orange))))

   ;; ctable
   `(ctbl:face-cell-select ((t (:background ,monokai-highlight-line
                                            :foreground ,monokai-emphasis
                                            :underline ,monokai-emphasis))))
   `(ctbl:face-continue-bar ((t (:background ,monokai-gray
                                             :foreground ,monokai-yellow))))
   `(ctbl:face-row-select ((t (:background ,monokai-highlight-line
                                           :foreground ,monokai-foreground
                                           :underline t))))

   ;; coffee
   `(coffee-mode-class-name ((t (:foreground ,monokai-yellow))))
   `(coffee-mode-function-param ((t (:foreground ,monokai-violet :slant italic))))

   ;; custom
   `(custom-face-tag ((t (:inherit ,monokai-pitch
                                   :height ,monokai-height-plus-3
                                   :foreground ,monokai-violet))))
   `(custom-variable-tag ((t (:inherit ,monokai-pitch
                                       :foreground ,monokai-cyan-l
                                       :height ,monokai-height-plus-3))))

   `(custom-comment-tag ((t (:foreground ,monokai-comments))))
   `(custom-group-tag ((t (:inherit ,monokai-pitch
                                    :foreground ,monokai-blue
                                    :height ,monokai-height-plus-3))))
   `(custom-group-tag-1 ((t (:inherit ,monokai-pitch
                                      :foreground ,monokai-red
                                      :height ,monokai-height-plus-3))))

   `(custom-state ((t (:foreground ,monokai-green))))

   ;; diff
   `(diff-added ((t (:foreground ,monokai-green
                                 :background ,monokai-background))))
   `(diff-changed ((t (:foreground ,monokai-blue
                                   :background ,monokai-background))))
   `(diff-removed ((t (:foreground ,monokai-red
                                   :background ,monokai-background))))

   `(diff-header ((t (:background ,monokai-background))))
   `(diff-file-header ((t (:background ,monokai-background
                                       :foreground ,monokai-foreground))))
   `(diff-refine-added ((t (:foreground ,monokai-background
                                        :background ,monokai-green))))
   `(diff-refine-change ((t (:foreground ,monokai-background
                                         :background ,monokai-blue))))
   `(diff-refine-removed ((t (:foreground ,monokai-background
                                          :background ,monokai-red))))

   ;; diff-hl
   `(diff-hl-change ((t (:background ,monokai-yellow-hc-alt
                                     :foreground ,monokai-yellow-hc-alt))))
   `(diff-hl-delete ((t (:background ,monokai-red-hc-alt
                                     :foreground ,monokai-red-hc-alt))))
   `(diff-hl-insert ((t (:background ,monokai-green-hc-alt
                                     :foreground ,monokai-green-hc-alt))))
   `(diff-hl-unknown ((t (:background ,monokai-violet-hc
                                      :foreground ,monokai-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A ((t (:background ,monokai-diff-red-emphasis))))
   `(ediff-fine-diff-B ((t (:background ,monokai-diff-green-emphasis))))
   `(ediff-fine-diff-C ((t (:background ,monokai-diff-blue-emphasis))))

   `(ediff-current-diff-A ((t (:background ,monokai-diff-red-base))))
   `(ediff-current-diff-B ((t (:background ,monokai-diff-green-base))))
   `(ediff-current-diff-C ((t (:background ,monokai-diff-blue-base))))

   `(ediff-even-diff-A ((t (:background ,monokai-comments
                                        :foreground ,monokai-foreground-lc ))))
   `(ediff-odd-diff-A  ((t (:background ,monokai-comments
                                        :foreground ,monokai-foreground-hc ))))
   `(ediff-even-diff-B ((t (:background ,monokai-comments
                                        :foreground ,monokai-foreground-hc ))))
   `(ediff-odd-diff-B  ((t (:background ,monokai-comments
                                        :foreground ,monokai-foreground-lc ))))
   `(ediff-even-diff-C ((t (:background ,monokai-comments
                                        :foreground ,monokai-foreground ))))
   `(ediff-odd-diff-C  ((t (:background ,monokai-comments
                                        :foreground ,monokai-background ))))

   ;; elixir
   `(elixir-attribute-face ((t (:foreground ,monokai-orange))))
   `(elixir-atom-face      ((t (:foreground ,monokai-violet))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-heredoc-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-regexp-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-op-face ((t (:inherit font-lock-keyword-face))))

   ;; epc
   `(epc:face-title ((t (:foreground ,monokai-blue
                                     :background ,monokai-background
                                     :weight normal
                                     :underline nil))))

   ;; eshell
   `(eshell-prompt        ((t (:foreground ,monokai-blue :inherit bold))))
   `(eshell-ls-archive    ((t (:foreground ,monokai-red))))
   `(eshell-ls-backup     ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter    ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory  ((t (:foreground ,monokai-blue :inherit bold))))
   `(eshell-ls-executable ((t (:foreground ,monokai-green :inherit bold))))
   `(eshell-ls-unreadable ((t (:foreground ,monokai-foreground))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,monokai-yellow :inherit bold))))
   `(eshell-ls-symlink ((t (:foreground ,monokai-cyan-l :inherit bold))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-class (:foreground ,monokai-red-hc
                                   :background ,monokai-red-lc
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-256-class (:foreground ,monokai-256-red-hc
                                       :background ,monokai-256-red-lc
                                       :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-class (:foreground ,monokai-green-hc
                                   :background ,monokai-green-lc))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-256-class (:foreground ,monokai-256-green-hc
                                       :background ,monokai-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-class (:foreground ,monokai-yellow-hc
                                   :background ,monokai-yellow-lc
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,monokai-256-class (:foreground ,monokai-256-yellow-hc
                                       :background ,monokai-256-yellow-lc
                                       :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-red)))
      (,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-red)))
      (,monokai-256-class (:foreground ,monokai-256-red
                                       :background ,monokai-256-background
                                       :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-orange)))
      (,monokai-class (:foreground ,monokai-orange
                                   :background ,monokai-background
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-orange)))
      (,monokai-256-class (:foreground ,monokai-256-orange
                                       :background ,monokai-256-background
                                       :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) monokai-class)
       (:underline (:style line :color ,monokai-blue)))
      (,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background
                                   :underline t))
      (,(append '((supports :underline (:style line))) monokai-256-class )
       (:underline (:style line :color ,monokai-256-blue)))
      (,monokai-256-class (:foreground ,monokai-256-blue
                                       :background ,monokai-256-background
                                       :underline t))))

   `(flycheck-fringe-error ((t (:foreground ,monokai-red-l
                                            :background unspecified))))

   `(flycheck-fringe-warning ((t (:foreground ,monokai-orange-l
                                              :background unspecified))))

   `(flycheck-fringe-info ((t (:foreground ,monokai-blue-l
                                           :background unspecified))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-yellow)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-yellow
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-yellow)
                   :inherit unspecified))
      (,monokai-256-class (:foreground ,monokai-256-yellow
                                       :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) monokai-class)
       (:underline (:style wave :color ,monokai-red)
                   :inherit unspecified))
      (,monokai-class (:foreground ,monokai-red
                                   :underline t))
      (,(append '((supports :underline (:style wave))) monokai-256-class )
       (:underline (:style wave :color ,monokai-256-red)
                   :inherit unspecified))
      (,monokai-256-class (:foreground ,monokai-256-red
                                       :underline t))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face ((t (:foreground ,monokai-blue
                                                             :background ,monokai-highlight-line
                                                             :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,monokai-blue))))
   `(guide-key/key-face ((t (:foreground ,monokai-orange))))
   `(guide-key/prefix-command-face ((t (:foreground ,monokai-violet))))

   ;; hi-lock-mode
   `(hi-yellow ((t (:foreground ,monokai-yellow-lc
                                :background ,monokai-yellow-hc))))
   `(hi-pink ((t (:foreground ,monokai-magenta-lc
                              :background ,monokai-magenta-hc))))
   `(hi-green ((t (:foreground ,monokai-green-lc
                               :background ,monokai-green-hc))))
   `(hi-blue ((t (:foreground ,monokai-blue-lc
                              :background ,monokai-blue-hc))))
   `(hi-black-b ((t (:foreground ,monokai-emphasis
                                 :background ,monokai-background))))

   `(hi-blue-b ((t (:foreground ,monokai-blue-lc))))
   `(hi-green-b ((t (:foreground ,monokai-green-lc))))
   `(hi-red-b ((t (:foreground ,monokai-red))))
   `(hi-black-hb ((t (:foreground ,monokai-emphasis
                                  :background ,monokai-background))))

   ;; highlight-changes
   `(highlight-changes ((t (:foreground ,monokai-orange))))
   `(highlight-changes-delete ((t (:foreground ,monokai-red :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face ((t (:background ,monokai-gray))))
   `(highlight-indentation-current-column-face ((t (:background ,monokai-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face ((t (:background ,monokai-highlight))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,monokai-highlight-line))))
   `(hl-line-face ((t (:background ,monokai-highlight-line))))

   ;; ido-mode
   `(ido-first-match ((t (:foreground ,monokai-yellow :weight normal))))
   `(ido-only-match ((t (:foreground ,monokai-background
                                     :background ,monokai-yellow
                                     :weight normal))))

   `(ido-subdir ((t (:foreground ,monokai-blue))))
   `(ido-incomplete-regexp ((t (:foreground ,monokai-red))))
   `(ido-indicator ((t (:background ,monokai-red
                                    :foreground ,monokai-background
                                    :width condensed))))

   `(ido-virtual ((t (:foreground ,monokai-cyan-l))))

   ;; info
   `(info-header-xref ((t (:foreground ,monokai-green
                                       :inherit bold
                                       :underline t))))

   `(info-menu ((t (:foreground ,monokai-blue))))
   `(info-node ((t (:foreground ,monokai-violet :inherit bold))))
   `(info-quoted-name ((t (:foreground ,monokai-orange))))
   `(info-reference-item ((t (:background unspecified
                                          :underline t
                                          :inherit bold))))

   `(info-string ((t (:foreground ,monokai-yellow))))

   `(info-title-1 ((t (:height ,monokai-height-plus-4))))
   `(info-title-2 ((t (:height ,monokai-height-plus-3))))
   `(info-title-3 ((t (:height ,monokai-height-plus-2))))
   `(info-title-4 ((t (:height ,monokai-height-plus-1))))

   ;; js2-mode colors
   `(js2-error ((t (:foreground ,monokai-red))))
   `(js2-external-variable ((t (:foreground ,monokai-orange))))
   `(js2-function-call ((t (:foreground ,monokai-foreground))))
   `(js2-function-param ((t (:foreground ,monokai-orange))))
   `(js2-instance-member ((t (:foreground ,monokai-violet))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,monokai-green))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,monokai-green))))
   `(js2-jsdoc-tag ((t (:foreground ,monokai-violet))))
   `(js2-jsdoc-type ((t (:foreground ,monokai-blue))))
   `(js2-jsdoc-value ((t (:foreground ,monokai-orange))))
   `(js2-magic-paren ((t (:underline t))))
   `(js2-object-property ((t (:foreground ,monokai-foreground))))
   `(js2-private-function-call ((t (:foreground ,monokai-violet))))
   `(js2-private-member ((t (:foreground ,monokai-blue))))
   `(js2-warning ((t (:underline ,monokai-orange))))

   ;; jedi
   `(jedi:highlight-function-argument ((t (:inherit bold))))

   ;; linum-mode
   `(linum ((t (:foreground ,monokai-line-number
                            :background ,monokai-fringe-bg
                            :inherit default
                            :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number ((t (:foreground ,monokai-line-number
                                  :background ,monokai-fringe-bg
                                  :inherit default
                                  :underline nil))))
   `(line-number-current-line ((t (:foreground ,monokai-foreground
                                               :background ,monokai-fringe-bg
                                               :inherit default
                                               :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face ((t (:foreground ,monokai-line-number
                                                  :background ,monokai-highlight-line
                                                  :underline nil))))

   ;; lsp-mode
   `(lsp-ui-doc-header ((t (:inherit org-document-title))))

   `(lsp-ui-doc-background ((t (:background ,monokai-highlight-line))))

   ;; magit
   `(magit-bisect-good ((t (:foreground ,monokai-green))))
   `(magit-bisect-skip ((t (:foreground ,monokai-orange))))
   `(magit-bisect-bad ((t (:foreground ,monokai-red))))

   `(magit-blame-highlight ((t (:foreground ,monokai-foreground
                                            :background ,monokai-highlight-alt))))

   `(magit-diff-file-heading-selection ((t (:inherit magit-diff-file-heading-highlight
                                                     :foreground ,monokai-orange-d))))
   `(magit-diff-hunk-heading ((t (:foreground ,monokai-gray-d
                                              :background ,monokai-gray-l))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,monokai-background
                                                        :background ,monokai-foreground))))
   `(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight
                                                     :foreground ,monokai-orange))))
   `(magit-diff-lines-heading ((t (:inherit magit-diff-hunk-heading-highlight
                                            :foreground ,monokai-background
                                            :background ,monokai-orange-l))))
   `(magit-diff-added ((t (:foreground ,monokai-green
                                       :background ,monokai-background))))
   `(magit-diff-removed ((t (:foreground ,monokai-red
                                         :background ,monokai-background))))
   `(magit-diff-base ((t (:foreground ,monokai-yellow
                                      :background ,monokai-background))))

   `(magit-diff-context ((t (:foreground ,monokai-gray-l))))
   `(magit-diff-added-highlight ((t (:foreground ,monokai-green
                                                 :background ,monokai-highlight-alt))))
   `(magit-diff-removed-highlight ((t (:foreground ,monokai-red
                                                   :background ,monokai-highlight-alt))))
   `(magit-diff-base-highlight ((t (:foreground ,monokai-yellow
                                                :background ,monokai-highlight-alt))))
   `(magit-diff-context-highlight ((t (:foreground ,monokai-foreground
                                                   :background ,monokai-highlight-alt))))

   `(magit-diffstat-added ((t (:foreground ,monokai-green))))
   `(magit-diffstat-removed ((t (:foreground ,monokai-red))))

   `(magit-log-graph ((t (:foreground ,monokai-comments))))
   `(magit-log-author ((t (:foreground ,monokai-red-d
                                       :slant normal
                                       :weight normal))))
   `(magit-log-date ((t (:foreground ,monokai-gray
                                     :slant normal
                                     :weight normal))))

   `(magit-process-ok ((t (:inherit magit-section-heading
                                    :foreground ,monokai-green))))
   `(magit-process-ng ((t (:inherit magit-section-heading
                                    :foreground ,monokai-red))))

   `(magit-reflog-commit ((t (:foreground ,monokai-green))))
   `(magit-reflog-amend ((t (:foreground ,monokai-magenta))))
   `(magit-reflog-merge ((t (:foreground ,monokai-green))))
   `(magit-reflog-checkout ((t (:foreground ,monokai-blue))))
   `(magit-reflog-reset ((t (:foreground ,monokai-red))))
   `(magit-reflog-rebase ((t (:foreground ,monokai-violet))))
   `(magit-reflog-cherry-pick ((t (:foreground ,monokai-green))))
   `(magit-reflog-remote ((t (:foreground ,monokai-cyan-l))))
   `(magit-reflog-other ((t (:foreground ,monokai-cyan-l))))

   `(magit-section-highlight ((t (:background ,monokai-highlight-line))))
   `(magit-section-heading ((t (:foreground ,monokai-yellow))))
   `(magit-section-heading-selection ((t (:foreground ,monokai-orange))))

   `(magit-sequence-stop ((t (:foreground ,monokai-cyan-l))))
   `(magit-sequence-part ((t (:foreground ,monokai-orange))))
   `(magit-sequence-head ((t (:foreground ,monokai-blue))))
   `(magit-sequence-drop ((t (:foreground ,monokai-red))))

   `(magit-dimmed ((t (:foreground ,monokai-comments))))
   `(magit-hash ((t (:foreground ,monokai-comments))))
   `(magit-tag ((t (:foreground ,monokai-orange))))

   `(magit-branch-remote ((t (:foreground ,monokai-green))))
   `(magit-branch-local ((t (:foreground ,monokai-blue))))

   `(magit-refname ((t (:foreground ,monokai-comments))))

   `(magit-signature-good ((t (:foreground ,monokai-green-d))))
   `(magit-signature-bad ((t (:foreground ,monokai-red-d))))
   `(magit-signature-untrusted ((t (:foreground ,monokai-cyan-l))))
   `(magit-signature-expired ((t (:foreground ,monokai-orange))))
   `(magit-signature-revoked ((t (:foreground ,monokai-magenta))))
   `(magit-signature-error ((t (:foreground ,monokai-red-l))))

   `(magit-cherry-unmatched ((t (:foreground ,monokai-cyan-l))))
   `(magit-cherry-equivalent ((t (:foreground ,monokai-magenta))))

   ;; man
   `(Man-overstrike ((t (:foreground ,monokai-blue))))
   `(Man-reverse ((t (:foreground ,monokai-orange))))
   `(Man-underline ((t (:foreground ,monokai-green :underline t))))

   ;; monky
   `(monky-section-title ((t (:foreground ,monokai-yellow))))
   `(monky-diff-add ((t (:foreground ,monokai-green))))
   `(monky-diff-del ((t (:foreground ,monokai-red))))

   ;; markdown-mode
   `(markdown-header-face ((t (:foreground ,monokai-green))))
   `(markdown-header-face-1 ((t (:inherit markdown-header-face))))
   `(markdown-header-face-2 ((t (:inherit markdown-header-face))))
   `(markdown-header-face-3 ((t (:inherit markdown-header-face))))
   `(markdown-header-face-4 ((t (:inherit markdown-header-face))))
   `(markdown-header-face-5 ((t (:inherit markdown-header-face))))
   `(markdown-header-face-6 ((t (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text ((t (:foreground ,monokai-comments))))
   `(message-header-name ((t (:foreground ,monokai-comments))))
   `(message-header-other ((t (:foreground ,monokai-foreground :weight normal))))
   `(message-header-to ((t (:foreground ,monokai-foreground :weight normal))))
   `(message-header-cc ((t (:foreground ,monokai-foreground :weight normal))))
   `(message-header-newsgroups ((t (:foreground ,monokai-yellow))))
   `(message-header-subject ((t (:foreground ,monokai-cyan-l :weight normal))))
   `(message-header-xheader ((t (:foreground ,monokai-cyan-l))))
   `(message-mml ((t (:foreground ,monokai-yellow))))
   `(message-separator ((t (:foreground ,monokai-comments :slant italic))))

   ;; mingus
   `(mingus-directory-face ((t (:foreground ,monokai-blue))))
   `(mingus-pausing-face ((t (:foreground ,monokai-magenta))))
   `(mingus-playing-face ((t (:foreground ,monokai-cyan-l))))
   `(mingus-playlist-face ((t (:foreground ,monokai-cyan-l ))))
   `(mingus-song-file-face ((t (:foreground ,monokai-yellow))))
   `(mingus-stopped-face ((t (:foreground ,monokai-red))))

   ;; moccur
   `(moccur-current-line-face ((t (:underline t))))
   `(moccur-edit-done-face ((t (:foreground ,monokai-comments
                                            :background ,monokai-background
                                            :slant italic))))
   `(moccur-edit-face ((t (:background ,monokai-yellow
                                       :foreground ,monokai-background))))
   `(moccur-edit-file-face ((t (:background ,monokai-highlight-line))))
   `(moccur-edit-reject-face ((t (:foreground ,monokai-red))))
   `(moccur-face ((t (:background ,monokai-highlight-line
                                  :foreground ,monokai-emphasis))))
   `(search-buffers-face ((t (:background ,monokai-highlight-line
                                          :foreground ,monokai-emphasis))))
   `(search-buffers-header-face ((t (:background ,monokai-highlight-line
                                                 :foreground ,monokai-yellow))))

   ;; neo-tree
   `(neo-banner-face ((t (:foreground ,monokai-blue
                                      :background ,monokai-background))))
   `(neo-header-face ((t (:foreground ,monokai-emphasis
                                      :background ,monokai-background))))
   `(neo-root-dir-face ((t (:foreground ,monokai-green
                                        :background ,monokai-background))))
   `(neo-dir-link-face ((t (:foreground ,monokai-blue))))
   `(neo-file-link-face ((t (:foreground ,monokai-foreground))))
   `(neo-button-face ((t (:underline nil))))
   `(neo-expand-btn-face ((t (:foreground ,monokai-comments))))
   `(neo-vc-default-face ((t (:foreground ,monokai-foreground))))
   `(neo-vc-user-face ((t (:foreground ,monokai-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,monokai-comments))))
   `(neo-vc-edited-face ((t (:foreground ,monokai-orange))))
   `(neo-vc-needs-update-face ((t (:underline t))))
   `(neo-vc-needs-merge-face ((t (:foreground ,monokai-red))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,monokai-red
                                                   :background ,monokai-comments))))
   `(neo-vc-added-face ((t (:foreground ,monokai-green))))
   `(neo-vc-removed-face ((t (:strike-through t))))
   `(neo-vc-conflict-face ((t (:foreground ,monokai-red))))
   `(neo-vc-missing-face ((t (:foreground ,monokai-red))))
   `(neo-vc-ignored-face ((t (:foreground ,monokai-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face ((t (:foreground ,monokai-gray-l))))
   `(markup-table-face ((t (:foreground ,monokai-blue-hc
                                        :background ,monokai-blue-lc))))
   `(markup-verbatim-face ((t (:background ,monokai-orange-lc))))
   `(markup-list-face ((t (:foreground ,monokai-violet-hc
                                       :background ,monokai-violet-lc))))
   `(markup-replacement-face ((t (:foreground ,monokai-violet))))
   `(markup-complex-replacement-face ((t (:foreground ,monokai-violet-hc
                                                      :background ,monokai-violet-lc))))
   `(markup-gen-face ((t (:foreground ,monokai-blue))))
   `(markup-secondary-text-face ((t (:foreground ,monokai-red))))

   ;; org-mode
   `(org-agenda-structure ((t (:foreground ,monokai-emphasis
                                           :background ,monokai-highlight-line
                                           :slant normal
                                           :inverse-video nil
                                           :height ,monokai-height-plus-1
                                           :underline nil
                                           :box (:line-width 2 :color ,monokai-background)))))

   `(org-agenda-calendar-event ((t (:foreground ,monokai-emphasis))))
   `(org-agenda-calendar-sexp ((t (:foreground ,monokai-foreground
                                               :slant italic))))
   `(org-agenda-date ((t (:foreground ,monokai-comments
                                      :background ,monokai-background
                                      :weight normal
                                      :inverse-video nil
                                      :overline nil
                                      :slant normal
                                      :height 1.0
                                      :box (:line-width 2 :color ,monokai-background)))) t)

   `(org-agenda-date-weekend ((t (:inherit org-agenda-date
                                           :inverse-video nil
                                           :background unspecified
                                           :foreground ,monokai-comments
                                           :weight unspecified
                                           :underline t
                                           :overline nil
                                           :box unspecified))) t)

   `(org-agenda-date-today ((t (:inherit org-agenda-date
                                         :inverse-video t
                                         :underline unspecified
                                         :overline nil
                                         :box unspecified
                                         :foreground ,monokai-blue
                                         :background ,monokai-background))) t)

   `(org-agenda-done ((t (:foreground ,monokai-comments :slant italic))) t)
   `(org-archived ((t (:foreground ,monokai-comments :weight normal))))

   `(org-block ((t (:foreground ,monokai-violet
                                :background ,monokai-highlight-alt))))
   `(org-block-background ((t (:background ,monokai-highlight-alt))))
   `(org-block-begin-line ((t (:foreground ,monokai-comments
                                           :background ,monokai-gray-d
                                           :slant italic))))
   `(org-block-end-line ((t (:foreground ,monokai-comments
                                         :background ,monokai-gray-d
                                         :slant italic))))
   `(org-checkbox ((t (:background ,monokai-background
                                   :foreground ,monokai-foreground
                                   :box (:line-width 1 :style released-button)))))

   `(org-code ((t (:foreground ,monokai-orange))))
   `(org-date ((t (:foreground ,monokai-blue :underline t))))
   `(org-done ((t (:foreground ,monokai-green))))
   `(org-ellipsis ((t (:foreground ,monokai-comments))))
   `(org-formula ((t (:foreground ,monokai-yellow))))
   `(org-headline-done ((t (:foreground ,monokai-green))))
   `(org-hide ((t (:foreground ,monokai-background))))

   `(org-level-1 ((t (:inherit ,monokai-pitch :foreground ,monokai-orange))))
   `(org-level-2 ((t (:inherit ,monokai-pitch :foreground ,monokai-green))))
   `(org-level-3 ((t (:inherit ,monokai-pitch :foreground ,monokai-blue))))
   `(org-level-4 ((t (:inherit ,monokai-pitch :foreground ,monokai-yellow))))
   `(org-level-5 ((t (:inherit ,monokai-pitch :foreground ,monokai-cyan-l))))
   `(org-level-6 ((t (:inherit ,monokai-pitch :foreground ,monokai-green))))
   `(org-level-7 ((t (:inherit ,monokai-pitch :foreground ,monokai-red))))
   `(org-level-8 ((t (:inherit ,monokai-pitch :foreground ,monokai-blue))))

   `(org-link ((t (:foreground ,monokai-magenta-l :underline t))))
   `(org-list-dt ((t (:bold nil :foreground ,monokai-red))))
   `(org-sexp-date ((t (:foreground ,monokai-violet))))

   `(org-scheduled ((t (:foreground ,monokai-green))))
   `(org-scheduled-previously ((t (:foreground ,monokai-cyan-l))))
   `(org-scheduled-today ((t (:foreground ,monokai-blue :weight normal))))

   `(org-special-keyword ((t (:foreground ,monokai-comments))))
   `(org-table ((t (:foreground ,monokai-green))))
   `(org-tag ((t (:weight bold))))
   `(org-time-grid ((t (:foreground ,monokai-comments))))
   `(org-todo ((t (:foreground ,monokai-red))))
   `(org-upcoming-deadline ((t (:foreground ,monokai-yellow
                                            :weight normal
                                            :underline nil))))
   `(org-warning ((t (:foreground ,monokai-orange
                                  :weight normal
                                  :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face ((t (:background ,monokai-blue-lc :foreground ,monokai-blue-hc))))
   `(org-habit-clear-future-face ((t (:background ,monokai-blue-lc))))
   `(org-habit-ready-face ((t (:background ,monokai-green-lc :foreground ,monokai-green))))
   `(org-habit-ready-future-face ((t (:background ,monokai-green-lc))))
   `(org-habit-alert-face ((t (:background ,monokai-yellow :foreground ,monokai-yellow-lc))))
   `(org-habit-alert-future-face ((t (:background ,monokai-yellow-lc))))
   `(org-habit-overdue-face ((t (:background ,monokai-red :foreground ,monokai-red-lc))))
   `(org-habit-overdue-future-face ((t (:background ,monokai-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face ((t (:foreground ,monokai-comments))))
   `(org-agenda-restriction-lock ((t (:background ,monokai-yellow))))

   `(org-clock-overlay ((t (:background ,monokai-yellow))))

   `(org-column ((t (:background ,monokai-highlight-line
                                 :strike-through nil
                                 :underline nil
                                 :slant normal
                                 :weight normal
                                 :inherit default))))
   `(org-column-title ((t (:background ,monokai-highlight-line
                                       :underline t))))

   `(org-date-selected ((t (:foreground ,monokai-red
                                        :inverse-video t))))

   `(org-document-info ((t (:foreground ,monokai-blue))))
   `(org-document-title ((t (:foreground ,monokai-blue))))

   `(org-drawer ((t (:foreground ,monokai-cyan-l))))
   `(org-footnote ((t (:foreground ,monokai-orange-lc :underline t))))
   `(org-latex-and-export-specials ((t (:foreground ,monokai-orange))))
   `(org-mode-line-clock-overrun ((t (:inherit mode-line))))

   ;; outline
   `(outline-1 ((t (:inherit org-level-1))))
   `(outline-2 ((t (:inherit org-level-2))))
   `(outline-3 ((t (:inherit org-level-3))))
   `(outline-4 ((t (:inherit org-level-4))))
   `(outline-5 ((t (:inherit org-level-5))))
   `(outline-6 ((t (:inherit org-level-6))))
   `(outline-7 ((t (:inherit org-level-7))))
   `(outline-8 ((t (:inherit org-level-8))))

   ;; parenface
   `(paren-face ())

   ;; perspective
   `(persp-selected-face ((t (:foreground ,monokai-blue))))

   ;; pretty-mode
   `(pretty-mode-symbol-face ((t (:foreground ,monokai-yellow
                                              :weight normal))))

   ;; popup
   `(popup-face ((t (:background ,monokai-highlight-line :foreground ,monokai-foreground))))
   `(popup-isearch-match ((t (:background ,monokai-green))))
   `(popup-menu-face ((t (:background ,monokai-highlight-line :foreground ,monokai-foreground))))
   `(popup-menu-mouse-face ((t (:background ,monokai-blue :foreground ,monokai-foreground))))
   `(popup-menu-selection-face ((t (:background ,monokai-magenta :foreground ,monokai-background))))
   `(popup-scroll-bar-background-face ((t (:background ,monokai-comments))))
   `(popup-scroll-bar-foreground-face ((t (:background ,monokai-emphasis))))
   `(popup-tip-face ((t (:background ,monokai-highlight-line :foreground ,monokai-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,monokai-violet))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,monokai-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,monokai-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,monokai-yellow))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,monokai-orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,monokai-red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,monokai-violet))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,monokai-blue))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,monokai-green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,monokai-yellow))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,monokai-orange))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,monokai-red))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,monokai-foreground
                                                        :background ,monokai-background
                                                        :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,monokai-green-d))))
   `(realgud-overlay-arrow2 ((t (:foreground ,monokai-yellow-d))))
   `(realgud-overlay-arrow3 ((t (:foreground ,monokai-orange-d))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:foreground ,monokai-red-d))))
   `(realgud-bp-line-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-line-number ((t (:inerhit monokai-line-number))))
   `(realgud-backtrace-number ((t (:foreground ,monokai-yellow-d))))

   ;; sh-mode
   `(sh-quoted-exec ((t (:foreground ,monokai-violet))))
   `(sh-escaped-newline ((t (:foreground ,monokai-yellow))))
   `(sh-heredoc ((t (:foreground ,monokai-yellow))))

   ;; smartparens
   `(sp-pair-overlay-face ((t (:background ,monokai-highlight-line))))
   `(sp-wrap-overlay-face ((t (:background ,monokai-highlight-line))))
   `(sp-wrap-tag-overlay-face ((t (:background ,monokai-highlight-line))))
   `(sp-show-pair-enclosing ((t (:inherit highlight))))
   `(sp-show-pair-match-face ((t (:foreground ,monokai-green
                                              :background ,monokai-background
                                              :weight normal
                                              :inverse-video t))))

   `(sp-show-pair-mismatch-face ((t (:foreground ,monokai-red
                                                 :background ,monokai-background
                                                 :weight normal
                                                 :inverse-video t))))

   ;; show-paren
   `(show-paren-match ((t (:foreground ,monokai-green
                                       :background ,monokai-background
                                       :weight normal
                                       :inverse-video t))))
   `(show-paren-mismatch ((t (:foreground ,monokai-red
                                          :background ,monokai-background
                                          :weight normal
                                          :inverse-video t))))

   ;; mic-paren
   `(paren-face-match ((t (:foreground ,monokai-green
                                       :background ,monokai-background
                                       :weight normal
                                       :inverse-video t))))
   `(paren-face-mismatch ((t (:foreground ,monokai-red
                                          :background ,monokai-background
                                          :weight normal
                                          :inverse-video t))))
   `(paren-face-no-match ((t (:foreground ,monokai-red
                                          :background ,monokai-background
                                          :weight normal
                                          :inverse-video t))))

   ;; speedbar
   `(speedbar-button-face ((t (:inherit ,monokai-pitch :foreground ,monokai-comments))))
   `(speedbar-directory-face ((t (:inherit ,monokai-pitch :foreground ,monokai-blue))))
   `(speedbar-file-face ((t (:inherit ,monokai-pitch :foreground ,monokai-foreground))))
   `(speedbar-highlight-face ((t (:inherit ,monokai-pitch :background ,monokai-highlight-line))))
   `(speedbar-selected-face ((t (:inherit ,monokai-pitch
                                          :foreground ,monokai-yellow
                                          :underline t))))
   `(speedbar-separator-face ((t (:inherit ,monokai-pitch
                                           :background ,monokai-blue
                                           :foreground ,monokai-background
                                           :overline ,monokai-cyan-lc))))
   `(speedbar-tag-face ((t (:inherit ,monokai-pitch
                                     :foreground ,monokai-green))))

   ;; table
   `(table-cell ((t (:foreground ,monokai-foreground
                                 :background ,monokai-highlight-line))))

   ;; term
   `(term-color-black ((t (:foreground ,monokai-background :background ,monokai-highlight-line))))
   `(term-color-red ((t (:foreground ,monokai-red :background ,monokai-red-d))))
   `(term-color-green ((t (:foreground ,monokai-green :background ,monokai-green-d))))
   `(term-color-yellow ((t (:foreground ,monokai-yellow :background ,monokai-yellow-d))))
   `(term-color-blue ((t (:foreground ,monokai-blue :background ,monokai-blue-d))))
   `(term-color-magenta ((t (:foreground ,monokai-magenta :background ,monokai-magenta-d))))
   `(term-color-cyan ((t (:foreground ,monokai-cyan-l :background ,monokai-cyan-l))))
   `(term-color-white ((t (:foreground ,monokai-emphasis :background ,monokai-foreground))))
   `(term-default-fg-color ((t (:inherit term-color-white))))
   `(term-default-bg-color ((t (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip ((t (:background ,monokai-background
                              :foreground ,monokai-foreground
                              :inherit ,monokai-pitch))))

   ;; treemacs
   `(treemacs-directory-face ((t (:foreground ,monokai-violet
                                              :background ,monokai-background))))
   `(treemacs-header-face ((t (:foreground ,monokai-yellow
                                           :background ,monokai-background
                                           :underline t))))

   `(treemacs-git-modified-face ((t (:foreground ,monokai-green
                                                 :background ,monokai-background))))
   `(treemacs-git-renamed-face ((t (:foreground ,monokai-red
                                                :background ,monokai-background))))
   `(treemacs-git-ignored-face ((t (:foreground ,monokai-gray-l
                                                :background ,monokai-background))))
   `(treemacs-git-untracked-face ((t (:foreground ,monokai-red
                                                  :background ,monokai-background))))
   `(treemacs-git-added-face ((t (:foreground ,monokai-green
                                              :background ,monokai-background))))
   `(treemacs-git-conflict-face ((t (:foreground ,monokai-orange
                                                 :background ,monokai-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face ((t (:foreground ,monokai-magenta))))
   `(tuareg-font-lock-multistage-face ((t (:foreground ,monokai-blue
                                                       :background ,monokai-highlight-line))))

   `(tuareg-font-lock-operator-face ((t (:foreground ,monokai-emphasis))))
   `(tuareg-font-lock-error-face ((t (:foreground ,monokai-yellow
                                                  :background ,monokai-red))))

   `(tuareg-font-lock-interactive-output-face ((t (:foreground ,monokai-cyan-l))))
   `(tuareg-font-lock-interactive-error-face ((t (:foreground ,monokai-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((t (:foreground ,monokai-comments
                                                        :background ,monokai-background))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,monokai-green))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,monokai-blue
                                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,monokai-emphasis
                                                              :background ,monokai-background))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,monokai-yellow))))

   ;; volatile highlights
   `(vhl/default-face ((t (:background ,monokai-highlight-alt))))

   ;; web-mode
   `(web-mode-builtin-face ((t (:foreground ,monokai-red))))
   `(web-mode-comment-face ((t (:foreground ,monokai-comments))))
   `(web-mode-constant-face ((t (:foreground ,monokai-violet))))
   `(web-mode-current-element-highlight-face ((t (:underline unspecified
                                                             :weight unspecified
                                                             :background ,monokai-highlight-line))))

   `(web-mode-doctype-face ((t (:foreground ,monokai-comments
                                            :slant italic
                                            :weight bold))))

   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,monokai-green))))

   `(web-mode-html-attr-name-face ((t (:foreground ,monokai-blue))))
   `(web-mode-html-attr-custom-face ((t (:inherit web-mode-html-attr-name-face))))
   `(web-mode-html-attr-engine-face ((t (:inherit web-mode-block-delimiter-face))))
   `(web-mode-html-attr-equal-face ((t (:inherit web-mode-html-attr-name-face))))
   `(web-mode-html-attr-value-face ((t (:foreground ,monokai-yellow))))
   `(web-mode-html-tag-face ((t (:foreground ,monokai-green))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,monokai-gray))))

   `(web-mode-keyword-face ((t (:foreground ,monokai-red))))

   `(web-mode-preprocessor-face ((t (:foreground ,monokai-yellow
                                                 :slant normal
                                                 :weight unspecified))))

   `(web-mode-string-face ((t (:foreground ,monokai-yellow))))
   `(web-mode-type-face ((t (:inherit font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:foreground ,monokai-orange))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))

   `(web-mode-block-face ((t (:background unspecified))))
   `(web-mode-block-delimiter-face ((t (:inherit font-lock-preprocessor-face))))
   `(web-mode-block-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-block-control-face ((t (:inherit font-lock-preprocessor-face))))
   `(web-mode-block-string-face ((t (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face ((t (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-css-pseudo-class-face ((t (:inherit font-lock-builtin-face))))
   `(web-mode-css-color-face ((t (:inherit font-lock-builtin-face))))
   `(web-mode-css-filter-face ((t (:inherit font-lock-function-name-face))))
   `(web-mode-css-function-face ((t (:inherit font-lock-builtin-face))))
   `(web-mode-css-function-call-face ((t (:inherit font-lock-function-name-face))))
   `(web-mode-css-priority-face ((t (:inherit font-lock-builtin-face))))
   `(web-mode-css-property-name-face ((t (:inherit font-lock-variable-name-face))))
   `(web-mode-css-selector-face ((t (:inherit font-lock-keyword-face))))
   `(web-mode-css-string-face ((t (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face ((t (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-json-context-face ((t (:foreground ,monokai-violet))))
   `(web-mode-json-key-face ((t (:foreground ,monokai-violet))))
   `(web-mode-json-string-face ((t (:inherit web-mode-string-face))))

   `(web-mode-param-name-face ((t (:foreground ,monokai-foreground))))

   `(web-mode-part-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-part-face ((t (:inherit web-mode-block-face))))
   `(web-mode-part-string-face ((t (:inherit web-mode-string-face))))

   `(web-mode-symbol-face ((t (:foreground ,monokai-violet))))

   `(web-mode-whitespace-face ((t (:background ,monokai-red))))

   ;; whitespace-mode
   `(whitespace-space ((t (:background unspecified
                                       :foreground ,monokai-comments
                                       :inverse-video unspecified
                                       :slant italic))))
   `(whitespace-hspace ((t (:background unspecified
                                        :foreground ,monokai-emphasis
                                        :inverse-video unspecified))))
   `(whitespace-tab ((t (:background unspecified
                                     :foreground ,monokai-red
                                     :inverse-video unspecified))))
   `(whitespace-newline ((t(:background unspecified
                                        :foreground ,monokai-comments
                                        :inverse-video unspecified))))
   `(whitespace-trailing ((t (:background unspecified
                                          :foreground ,monokai-orange-lc
                                          :inverse-video t))))
   `(whitespace-line ((t (:background unspecified
                                      :foreground ,monokai-magenta
                                      :inverse-video unspecified))))
   `(whitespace-space-before-tab ((t (:background ,monokai-red-lc
                                                  :foreground unspecified
                                                  :inverse-video unspecified))))
   `(whitespace-indentation ((t (:background unspecified
                                             :foreground ,monokai-yellow
                                             :inverse-video unspecified))))
   `(whitespace-empty ((t (:background unspecified
                                       :foreground ,monokai-red-lc
                                       :inverse-video t))))
   `(whitespace-space-after-tab ((t (:background unspecified
                                                 :foreground ,monokai-orange
                                                 :inverse-video t))))

   ;; which-func-mode
   `(which-func ((t (:foreground ,monokai-green))))

   ;; which-key
   `(which-key-key-face ((t (:foreground ,monokai-green))))
   `(which-key-separator-face ((t (:foreground ,monokai-comments))))
   `(which-key-note-face ((t (:foreground ,monokai-comments))))
   `(which-key-command-description-face ((t (:foreground ,monokai-foreground))))
   `(which-key-local-map-description-face ((t (:foreground ,monokai-yellow-hc))))
   `(which-key-group-description-face ((t (:foreground ,monokai-red))))

   ;; window-divider-mode
   `(window-divider ((t (:foreground ,monokai-highlight))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

   ;; window-number-mode
   `(window-number-face ((t (:foreground ,monokai-green))))

   ;; zencoding
   `(zencoding-preview-input ((t (:background ,monokai-highlight-line
                                              :box ,monokai-emphasis)))))

  (custom-theme-set-variables
   'monokai
   `(ansi-color-names-vector [,monokai-background ,monokai-red ,monokai-green ,monokai-yellow
                                                  ,monokai-blue ,monokai-magenta ,monokai-cyan ,monokai-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,monokai-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,monokai-magenta ,monokai-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,monokai-highlight-line . 0)
       (,monokai-green-lc . 20)
       (,monokai-cyan-lc . 30)
       (,monokai-blue-lc . 50)
       (,monokai-yellow-lc . 60)
       (,monokai-orange-lc . 70)
       (,monokai-magenta-lc . 85)
       (,monokai-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,monokai-background)
   `(pos-tip-background-color ,monokai-yellow-hc)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,monokai-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,monokai-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,monokai-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,monokai-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,monokai-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monokai)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; monokai-theme.el ends here
