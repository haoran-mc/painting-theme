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
;;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.monokai.nl/blog/2006/07/15/textmate-color-theme/
;;
;; Bozhidar Batsov created zenburn-theme.el and solarized-theme.el
;;  on which this file is based.
;; - https://github.com/bbatsov/zenburn-emacs
;;
;; Color Scheme Designer 3 for complementary colours.
;; - http://colorschemedesigner.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;; K. Adam Christensen for his personal monokai theme that addresses 256 colours.
;; - https://github.com/pope/personal/blob/master/etc/emacs.d/monokai-theme.el
;;
;; Thomas Frössman for his work on solarized-emacs.
;; - http://github.com/bbatsov/solarized-emacs
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The monokai theme requires Emacs 24 or later!"))

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
   `(font-lock-builtin-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(font-lock-comment-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(font-lock-constant-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(font-lock-doc-face
     ((,monokai-class (:foreground ,(if monokai-doc-face-as-comment
                                        monokai-comments
                                      monokai-cyan)))))

   `(font-lock-function-name-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(font-lock-keyword-face
     ((,monokai-class (:foreground ,monokai-red
                                   :weight normal))))

   `(font-lock-negation-char-face
     ((,monokai-class (:foreground ,monokai-cyan))))

   `(font-lock-preprocessor-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(font-lock-regexp-grouping-construct
     ((,monokai-class (:foreground ,monokai-cyan
                                   :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,monokai-class (:foreground ,monokai-violet
                                   :weight normal))))

   `(font-lock-string-face
     ((,monokai-class (:foreground ,monokai-cyan))))

   `(font-lock-type-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :italic nil))))

   `(font-lock-variable-name-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(font-lock-warning-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight bold
                                   :italic t
                                   :underline t))))

   `(c-annotation-face
     ((,monokai-class (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,monokai-class (:foreground ,monokai-foreground
                                    :background ,monokai-background))))

   `(highlight
     ((,monokai-class (:background ,monokai-highlight))))

   `(lazy-highlight
     ((,monokai-class (:inherit highlight
                                :background ,monokai-highlight-alt))))

   `(region
     ((,monokai-class (:inherit highlight
                                :background ,monokai-highlight))))

   `(secondary-selection
     ((,monokai-class (:inherit region
                                :background ,monokai-highlight-alt))))

   `(shadow
     ((,monokai-class (:foreground ,monokai-comments))))

   `(match
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background))))

   `(cursor
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-foreground
                                   :inverse-video t))))

   `(mouse
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-foreground
                                   :inverse-video t))))

   `(escape-glyph
     ((,monokai-class (:foreground ,monokai-comments))))

   `(escape-glyph-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(fringe
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-fringe-bg))))

   `(link
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))))

   `(link-visited
     ((,monokai-class (:foreground ,monokai-violet
                                   :underline t
                                   :weight normal))))

   `(success
     ((,monokai-class (:foreground ,monokai-green))))

   `(warning
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(error
     ((,monokai-class (:foreground ,monokai-red))))

   `(eval-sexp-fu-flash
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))))

   `(eval-sexp-fu-flash-error
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-red))))

   `(trailing-whitespace
     ((,monokai-class (:background ,monokai-red))))

   `(vertical-border
     ((,monokai-class (:foreground ,monokai-gray))))

   `(menu
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-background))))

   `(minibuffer-prompt
     ((,monokai-class (:foreground ,monokai-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,monokai-class (:foreground ,monokai-green))))

   `(mode-line
     ((,monokai-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,monokai-emphasis
                                      :background ,monokai-highlight
                                      :box (:line-width 1
                                                        :color ,monokai-gray
                                                        :style nil)))))

   `(powerline-active1
     ((,monokai-class (:background ,monokai-gray-d))))

   `(powerline-active2
     ((,monokai-class (:background ,monokai-background))))


   `(mode-line-inactive
     ((,monokai-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,monokai-comments
                                      :background ,monokai-background
                                      :box (:line-width 1
                                                        :color ,monokai-gray
                                                        :style nil)))))

   `(powerline-inactive1
     ((,monokai-class (:background ,monokai-gray-d))))

   `(powerline-inactive2
     ((,monokai-class (:background ,monokai-background))))

   ;; header-line
   `(header-line
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-highlight
                                   :box (:color ,monokai-gray
                                                :line-width 1
                                                :style nil)))))

   ;; tab-line
   `(tab-line
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-highlight))))
   `(tab-line-highlight
     ((,monokai-class (:underline t))))
   `(tab-line-tab
     ((,monokai-class (:foreground ,monokai-foreground
				                   :background ,monokai-background
				                   :box (:line-width 4 :color ,monokai-background)))))
   `(tab-line-tab-current
     ((,monokai-class (:inherit tab-line-tab))))
   `(tab-line-tab-inactive
     ((,monokai-class (:inherit tab-line-tab
				                :foreground ,monokai-comments
				                :background ,monokai-highlight
				                :box (:line-width 4 :color ,monokai-highlight)))))
   `(tab-line-tab-inactive-alternate
     ((,monokai-class (:inherit tab-line-tab-inactive))))
   `(tab-line-tab-modified
     ((,monokai-class (:inherit tab-line-tab))))

   ;; tab-bar
   `(tab-bar
     ((,monokai-class (:background ,monokai-background))))
   `(tab-bar-tab
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-orange
                                   :box (:line-width -2 :color ,monokai-orange)))))
   `(tab-bar-tab-inactive
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-background))))

   ;; hl-todo
   `(hl-todo
     ((,monokai-class (:weight normal))))

   ;; window-tool-bar
   `(window-tool-bar-button
     ((,monokai-class (:inherit tab-line))))
   `(window-tool-bar-button-hover
     ((,monokai-class (:inherit tab-line :inverse-video t))))
   `(window-tool-bar-button-disabled
     ((,monokai-class (:inherit shadow :background ,monokai-highlight-alt))))

   ;; cua
   `(cua-global-mark
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background))))

   `(cua-rectangle
     ((,monokai-class (:inherit region))))

   `(cua-rectangle-noselect
     ((,monokai-class (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,monokai-class (:foreground ,monokai-yellow))))

   ;; dired
   `(dired-directory
     ((,monokai-class (:foreground ,monokai-blue))))

   `(dired-flagged
     ((,monokai-class (:foreground ,monokai-red))))

   `(dired-header
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-highlight))))

   `(dired-ignored
     ((,monokai-class (:inherit shadow))))

   `(dired-mark
     ((,monokai-class (:foreground ,monokai-green))))

   `(dired-marked
     ((,monokai-class (:foreground ,monokai-violet
                                   :inherit bold))))

   `(dired-perm-write
     ((,monokai-class (:foreground ,monokai-foreground
                                   :underline t))))

   `(dired-symlink
     ((,monokai-class (:foreground ,monokai-cyan-l
                                   :slant italic))))

   `(dired-warning
     ((,monokai-class (:foreground ,monokai-orange
                                   :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-blue))))

   `(dropdown-list-selection-face
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background))))

   ;; ee
   `(ee-bookmarked
     ((,monokai-class (:foreground ,monokai-emphasis))))

   `(ee-category
     ((,monokai-class (:foreground ,monokai-blue))))

   `(ee-link
     ((,monokai-class (:inherit link))))

   `(ee-link-visited
     ((,monokai-class (:inherit link-visited))))

   `(ee-marked
     ((,monokai-class (:foreground ,monokai-magenta))))

   `(ee-omitted
     ((,monokai-class (:foreground ,monokai-comments))))

   `(ee-shadow
     ((,monokai-class (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(grep-error-face
     ((,monokai-class (:foreground ,monokai-red
                                   :underline t))))

   `(grep-hit-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(grep-match-face
     ((,monokai-class (:foreground ,monokai-green))))

   ;; isearch
   `(isearch
     ((,monokai-class (:inherit region
                                :foreground ,monokai-background
                                :background ,monokai-yellow))))

   `(isearch-fail
     ((,monokai-class (:inherit isearch
                                :foreground ,monokai-red
                                :background ,monokai-background
                                :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background
                                   :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-background
                                   :inverse-video nil))))

   ;; auctex
   `(font-latex-bold-face
     ((,monokai-class (:inherit bold
                                :foreground ,monokai-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,monokai-class (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,monokai-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,monokai-class (:inherit italic :foreground ,monokai-emphasis))))

   `(font-latex-math-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(font-latex-sectioning-0-face
     ((,monokai-class (:inherit font-latex-sectioning-1-face
                                :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,monokai-class (:inherit font-latex-sectioning-2-face
                                :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,monokai-class (:inherit font-latex-sectioning-3-face
                                :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,monokai-class (:inherit font-latex-sectioning-4-face
                                :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,monokai-class (:inherit font-latex-sectioning-5-face
                                :height ,monokai-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-yellow))))

   `(font-latex-sedate-face
     ((,monokai-class (:foreground ,monokai-emphasis))))

   `(font-latex-slide-title-face
     ((,monokai-class (:inherit (,monokai-pitch font-lock-type-face)
                                :height ,monokai-height-plus-3))))

   `(font-latex-string-face
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(font-latex-subscript-face
     ((,monokai-class (:height ,monokai-height-minus-1))))

   `(font-latex-superscript-face
     ((,monokai-class (:height ,monokai-height-minus-1))))

   `(font-latex-verbatim-face
     ((,monokai-class (:inherit fixed-pitch
                                :foreground ,monokai-foreground
                                :slant italic))))

   `(font-latex-warning-face
     ((,monokai-class (:inherit bold
                                :foreground ,monokai-orange))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-blue))))

   `(ahs-edit-mode-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-highlight))))

   `(ahs-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-yellow))))

   `(ahs-plugin-bod-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-violet ))))

   `(ahs-plugin-defalt-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))))

   `(ahs-warning-face
     ((,monokai-class (:foreground ,monokai-red))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,monokai-class (:foreground ,monokai-violet))))

   ;; bm
   `(bm-face
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-background))))

   `(bm-fringe-face
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-background))))

   `(bm-fringe-persistent-face
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-background))))

   `(bm-persistent-face
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(cfw:face-annotation
     ((,monokai-class (:inherit cfw:face-day-title
                                :foreground ,monokai-yellow))))

   `(cfw:face-default-content
     ((,monokai-class (:foreground ,monokai-green))))

   `(cfw:face-default-day
     ((,monokai-class (:inherit cfw:face-day-title))))

   `(cfw:face-disable
     ((,monokai-class (:inherit cfw:face-day-title
                                :foreground ,monokai-comments))))

   `(cfw:face-grid
     ((,monokai-class (:foreground ,monokai-comments))))

   `(cfw:face-header
     ((,monokai-class (:foreground ,monokai-blue-hc
                                   :background ,monokai-blue-lc))))

   `(cfw:face-holiday
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-red))))

   `(cfw:face-periods
     ((,monokai-class (:foreground ,monokai-magenta))))

   `(cfw:face-select
     ((,monokai-class (:background ,monokai-magenta-lc
                                   :foreground ,monokai-magenta-hc))))

   `(cfw:face-saturday
     ((,monokai-class (:foreground ,monokai-cyan-hc
                                   :background ,monokai-cyan-lc))))

   `(cfw:face-sunday
     ((,monokai-class (:foreground ,monokai-red-hc
                                   :background ,monokai-red-lc))))

   `(cfw:face-title
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-yellow
                                :height ,monokai-height-plus-4))))

   `(cfw:face-today
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground unspecified))))

   `(cfw:face-today-title
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-yellow-hc))))

   `(cfw:face-toolbar
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))))

   `(cfw:face-toolbar-button-off
     ((,monokai-class (:background ,monokai-yellow-lc
                                   :foreground ,monokai-yellow-hc))))

   `(cfw:face-toolbar-button-on
     ((,monokai-class (:background ,monokai-yellow-hc
                                   :foreground ,monokai-yellow-lc))))

   ;; cider
   `(cider-enlightened
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background unspecified
                                   :box (:color ,monokai-yellow :line-width -1 :style nil)))))

   `(cider-enlightened-local
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(cider-instrumented-face
     ((,monokai-class (:foreground ,monokai-violet
                                   :background unspecified
                                   :box (:color ,monokai-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background unspecified
                                   :box (:color ,monokai-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-orange))))

   `(cider-test-failure-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-red))))

   `(cider-test-success-face
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))))

   `(cider-traced-face
     ((,monokai-class :box (:color ,monokai-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,monokai-class (:foreground ,monokai-red
                                   :underline t))))

   `(clojure-test-error-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :underline t))))

   `(clojure-test-success-face
     ((,monokai-class (:foreground ,monokai-green
                                   :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis))))

   `(company-tooltip-selection
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))))

   `(company-tooltip-mouse
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background))))

   `(company-tooltip-common
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))))

   `(company-tooltip-common-selection
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-blue
                                   :underline t))))

   `(company-preview
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis))))

   `(company-preview-common
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))))

   `(company-scrollbar-bg
     ((,monokai-class (:background ,monokai-gray))))

   `(company-scrollbar-fg
     ((,monokai-class (:background ,monokai-comments))))

   `(company-tooltip-annotation
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-green))))

   `(company-template-field
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-blue))))

   ;; compilation
   `(compilation-column-face
     ((,monokai-class (:foreground ,monokai-cyan-l
                                   :underline nil))))

   `(compilation-column-number
     ((,monokai-class (:inherit font-lock-doc-face
                                :foreground ,monokai-cyan-l
                                :underline nil))))

   `(compilation-enter-directory-face
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))))

   `(compilation-error
     ((,monokai-class (:inherit error
                                :underline nil))))

   `(compilation-error-face
     ((,monokai-class (:foreground ,monokai-red
                                   :underline nil))))

   `(compilation-face
     ((,monokai-class (:foreground ,monokai-foreground
                                   :underline nil))))

   `(compilation-info
     ((,monokai-class (:foreground ,monokai-comments
                                   :underline nil
                                   :bold nil))))

   `(compilation-info-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline nil))))

   `(compilation-leave-directory-face
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))))

   `(compilation-line-face
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))))

   `(compilation-line-number
     ((,monokai-class (:foreground ,monokai-green
                                   :underline nil))))

   `(compilation-warning
     ((,monokai-class (:inherit warning
                                :underline nil))))

   `(compilation-warning-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal
                                   :underline nil))))

   `(compilation-mode-line-exit
     ((,monokai-class (:inherit compilation-info
                                :foreground ,monokai-green))))

   `(compilation-mode-line-fail
     ((,monokai-class (:inherit compilation-error
                                :foreground ,monokai-red))))

   `(compilation-mode-line-run
     ((,monokai-class (:foreground ,monokai-orange))))

   ;; CSCOPE
   `(cscope-file-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(cscope-function-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(cscope-line-number-face
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(cscope-line-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(cscope-mouse-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis
                                   :underline ,monokai-emphasis))))

   `(ctbl:face-continue-bar
     ((,monokai-class (:background ,monokai-gray
                                   :foreground ,monokai-yellow))))

   `(ctbl:face-row-select
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground
                                   :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(coffee-mode-function-param
     ((,monokai-class (:foreground ,monokai-violet
                                   :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,monokai-class (:inherit ,monokai-pitch
                                :height ,monokai-height-plus-3
                                :foreground ,monokai-violet))))

   `(custom-variable-tag
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-cyan-l
                                :height ,monokai-height-plus-3))))

   `(custom-comment-tag
     ((,monokai-class (:foreground ,monokai-comments))))

   `(custom-group-tag
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue
                                :height ,monokai-height-plus-3))))

   `(custom-group-tag-1
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-red
                                :height ,monokai-height-plus-3))))

   `(custom-state
     ((,monokai-class (:foreground ,monokai-green))))

   ;; diff
   `(diff-added
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))))

   `(diff-changed
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background))))

   `(diff-removed
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background))))

   `(diff-header
     ((,monokai-class (:background ,monokai-background))))

   `(diff-file-header
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground))))

   `(diff-refine-added
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-green))))

   `(diff-refine-change
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-blue))))

   `(diff-refine-removed
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,monokai-class (:background ,monokai-yellow-hc-alt
                                   :foreground ,monokai-yellow-hc-alt))))

   `(diff-hl-delete
     ((,monokai-class (:background ,monokai-red-hc-alt
                                   :foreground ,monokai-red-hc-alt))))

   `(diff-hl-insert
     ((,monokai-class (:background ,monokai-green-hc-alt
                                   :foreground ,monokai-green-hc-alt))))

   `(diff-hl-unknown
     ((,monokai-class (:background ,monokai-violet-hc
                                   :foreground ,monokai-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,monokai-class (:background ,monokai-diff-red-emphasis))))

   `(ediff-fine-diff-B
     ((,monokai-class (:background ,monokai-diff-green-emphasis))))

   `(ediff-fine-diff-C
     ((,monokai-class (:background ,monokai-diff-blue-emphasis))))

   `(ediff-current-diff-A
     ((,monokai-class (:background ,monokai-diff-red-base))))

   `(ediff-current-diff-B
     ((,monokai-class (:background ,monokai-diff-green-base))))

   `(ediff-current-diff-C
     ((,monokai-class (:background ,monokai-diff-blue-base))))

   `(ediff-even-diff-A
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-hc ))))

   `(ediff-even-diff-B
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground-lc ))))

   `(ediff-even-diff-C
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-foreground ))))

   `(ediff-odd-diff-C
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-background ))))

   ;; elfeed
   `(elfeed-search-date-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(elfeed-search-feed-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(elfeed-search-tag-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(elfeed-search-title-face
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   ;; elixir
   `(elixir-attribute-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(elixir-atom-face
     ((,monokai-class (:foreground ,monokai-violet))))

   ;; ein
   `(ein:cell-input-area
     ((,monokai-class (:background ,monokai-highlight-line))))
   `(ein:cell-input-prompt
     ((,monokai-class (:foreground ,monokai-green))))
   `(ein:cell-output-prompt
     ((,monokai-class (:foreground ,monokai-red))))
   `(ein:notification-tab-normal
     ((,monokai-class (:foreground ,monokai-blue))))
   `(ein:notification-tab-selected
     ((,monokai-class (:foreground ,monokai-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,monokai-class (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,monokai-class (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,monokai-class (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,monokai-class (:inherit font-lock-keyword-face))))

   ;; epc
   `(epc:face-title
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background
                                   :weight normal
                                   :underline nil))))

   ;; eshell
   `(eshell-prompt
     ((,monokai-class (:foreground ,monokai-blue
                                   :inherit bold))))

   `(eshell-ls-archive
     ((,monokai-class (:foreground ,monokai-red))))

   `(eshell-ls-backup
     ((,monokai-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,monokai-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,monokai-class (:foreground ,monokai-blue
                                   :inherit bold))))

   `(eshell-ls-executable
     ((,monokai-class (:foreground ,monokai-green
                                   :inherit bold))))

   `(eshell-ls-unreadable
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(eshell-ls-missing
     ((,monokai-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,monokai-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,monokai-class (:foreground ,monokai-yellow
                                   :inherit bold))))

   `(eshell-ls-symlink
     ((,monokai-class (:foreground ,monokai-cyan-l
                                   :inherit bold))))

   ;; fic
   `(fic-author-face
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-orange
                                   :underline t
                                   :slant italic))))

   `(fic-face
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-orange
                                   :weight normal
                                   :slant italic))))

   `(font-lock-fic-face
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-orange
                                   :weight normal
                                   :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight normal
                                   :underline nil))))

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

   `(flycheck-fringe-error
     ((,monokai-class (:foreground ,monokai-red-l
                                   :background unspecified))))

   `(flycheck-fringe-warning
     ((,monokai-class (:foreground ,monokai-orange-l
                                   :background unspecified))))

   `(flycheck-fringe-info
     ((,monokai-class (:foreground ,monokai-blue-l
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


   ;; git-gutter
   `(git-gutter:added
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background
                                   :inherit bold))))

   `(git-gutter:deleted
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background
                                   :inherit bold))))

   `(git-gutter:modified
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background
                                   :inherit bold))))

   `(git-gutter:unchanged
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-background
                                   :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,monokai-class (:foreground ,monokai-green
                                   :inherit bold))))

   `(git-gutter-fr:deleted
     ((,monokai-class (:foreground ,monokai-red
                                   :inherit bold))))

   `(git-gutter-fr:modified
     ((,monokai-class (:foreground ,monokai-blue
                                   :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background
                                   :inherit bold))))

   `(git-gutter+-deleted
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background
                                   :inherit bold))))

   `(git-gutter+-modified
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background
                                   :inherit bold))))

   `(git-gutter+-unchanged
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-background
                                   :inherit bold))))

   `(git-gutter-fr+-added
     ((,monokai-class (:foreground ,monokai-green))))

   `(git-gutter-fr+-deleted
     ((,monokai-class (:foreground ,monokai-red))))

   `(git-gutter-fr+-modified
     ((,monokai-class (:foreground ,monokai-blue))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-highlight-line
                                   :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(guide-key/key-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(guide-key/prefix-command-face
     ((,monokai-class (:foreground ,monokai-violet))))

   ;; hi-lock-mode
   `(hi-yellow
     ((,monokai-class (:foreground ,monokai-yellow-lc
                                   :background ,monokai-yellow-hc))))

   `(hi-pink
     ((,monokai-class (:foreground ,monokai-magenta-lc
                                   :background ,monokai-magenta-hc))))

   `(hi-green
     ((,monokai-class (:foreground ,monokai-green-lc
                                   :background ,monokai-green-hc))))

   `(hi-blue
     ((,monokai-class (:foreground ,monokai-blue-lc
                                   :background ,monokai-blue-hc))))

   `(hi-black-b
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))))

   `(hi-blue-b
     ((,monokai-class (:foreground ,monokai-blue-lc))))

   `(hi-green-b
     ((,monokai-class (:foreground ,monokai-green-lc))))

   `(hi-red-b
     ((,monokai-class (:foreground ,monokai-red))))

   `(hi-black-hb
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))))

   ;; highlight-changes
   `(highlight-changes
     ((,monokai-class (:foreground ,monokai-orange))))

   `(highlight-changes-delete
     ((,monokai-class (:foreground ,monokai-red
                                   :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,monokai-class (:background ,monokai-gray))))

   `(highlight-indentation-current-column-face
     ((,monokai-class (:background ,monokai-gray))))

   ;; highlight-symbol
   `(highlight-symbol-face
     ((,monokai-class (:background ,monokai-highlight))))

   ;; hl-line-mode
   `(hl-line
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(hl-line-face
     ((,monokai-class (:background ,monokai-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal))))

   `(ido-only-match
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-yellow
                                   :weight normal))))

   `(ido-subdir
     ((,monokai-class (:foreground ,monokai-blue))))

   `(ido-incomplete-regexp
     ((,monokai-class (:foreground ,monokai-red ))))

   `(ido-indicator
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background
                                   :width condensed))))

   `(ido-virtual
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   ;; info
   `(info-header-xref
     ((,monokai-class (:foreground ,monokai-green
                                   :inherit bold
                                   :underline t))))

   `(info-menu
     ((,monokai-class (:foreground ,monokai-blue))))

   `(info-node
     ((,monokai-class (:foreground ,monokai-violet
                                   :inherit bold))))

   `(info-quoted-name
     ((,monokai-class (:foreground ,monokai-orange))))

   `(info-reference-item
     ((,monokai-class (:background unspecified
                                   :underline t
                                   :inherit bold))))

   `(info-string
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(info-title-1
     ((,monokai-class (:height ,monokai-height-plus-4))))

   `(info-title-2
     ((,monokai-class (:height ,monokai-height-plus-3))))

   `(info-title-3
     ((,monokai-class (:height ,monokai-height-plus-2))))

   `(info-title-4
     ((,monokai-class (:height ,monokai-height-plus-1))))

   ;; js2-mode colors
   `(js2-error
     ((,monokai-class (:foreground ,monokai-red))))

   `(js2-external-variable
     ((,monokai-class (:foreground ,monokai-orange))))

   `(js2-function-call
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(js2-function-param
     ((,monokai-class (:foreground ,monokai-orange))))

   `(js2-instance-member
     ((,monokai-class (:foreground ,monokai-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,monokai-class (:foreground ,monokai-green))))

   `(js2-jsdoc-html-tag-name
     ((,monokai-class (:foreground ,monokai-green))))

   `(js2-jsdoc-tag
     ((,monokai-class (:foreground ,monokai-violet))))

   `(js2-jsdoc-type
     ((,monokai-class (:foreground ,monokai-blue))))

   `(js2-jsdoc-value
     ((,monokai-class (:foreground ,monokai-orange))))

   `(js2-magic-paren
     ((,monokai-class (:underline t))))

   `(js2-object-property
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(js2-private-function-call
     ((,monokai-class (:foreground ,monokai-violet))))

   `(js2-private-member
     ((,monokai-class (:foreground ,monokai-blue))))

   `(js2-warning
     ((,monokai-class (:underline ,monokai-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,monokai-class (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,monokai-class (:foreground ,monokai-line-number
                                   :background ,monokai-fringe-bg
                                   :inherit default
                                   :underline nil))))

   ;; line-number (>= Emacs26)
   `(line-number
     ((,monokai-class (:foreground ,monokai-line-number
                                   :background ,monokai-fringe-bg
                                   :inherit default
                                   :underline nil))))
   `(line-number-current-line
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-fringe-bg
                                   :inherit default
                                   :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,monokai-class (:foreground ,monokai-line-number
                                   :background ,monokai-highlight-line
                                   :underline nil))))

   ;; lsp-mode
   `(lsp-ui-doc-header
     ((,monokai-class (:inherit org-document-title))))

   `(lsp-ui-doc-background
     ((,monokai-class (:background ,monokai-highlight-line))))

   ;; magit
   `(magit-bisect-good
     ((,monokai-class (:foreground ,monokai-green))))

   `(magit-bisect-skip
     ((,monokai-class (:foreground ,monokai-orange))))

   `(magit-bisect-bad
     ((,monokai-class (:foreground ,monokai-red))))

   `(magit-blame-highlight
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-highlight-alt))))

   `(magit-diff-file-heading-selection
     ((,monokai-class (:inherit magit-diff-file-heading-highlight
                                :foreground ,monokai-orange-d))))

   `(magit-diff-hunk-heading
     ((,monokai-class (:foreground ,monokai-gray-d
                                   :background ,monokai-gray-l))))

   `(magit-diff-hunk-heading-highlight
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-foreground))))

   `(magit-diff-hunk-heading-selection
     ((,monokai-class (:inherit magit-diff-hunk-heading-highlight
                                :foreground ,monokai-orange))))

   `(magit-diff-lines-heading
     ((,monokai-class (:inherit magit-diff-hunk-heading-highlight
                                :foreground ,monokai-background
                                :background ,monokai-orange-l))))

   `(magit-diff-added
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))))

   `(magit-diff-removed
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background))))

   `(magit-diff-base
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-background))))

   `(magit-diff-context
     ((,monokai-class (:foreground ,monokai-gray-l))))

   `(magit-diff-added-highlight
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-highlight-alt))))

   `(magit-diff-removed-highlight
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-highlight-alt))))

   `(magit-diff-base-highlight
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-highlight-alt))))

   `(magit-diff-context-highlight
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-highlight-alt))))

   `(magit-diffstat-added
     ((,monokai-class (:foreground ,monokai-green))))

   `(magit-diffstat-removed
     ((,monokai-class (:foreground ,monokai-red))))

   `(magit-log-graph
     ((,monokai-class (:foreground ,monokai-comments))))

   `(magit-log-author
     ((,monokai-class (:foreground ,monokai-red-d
                                   :slant normal
                                   :weight normal))))

   `(magit-log-date
     ((,monokai-class (:foreground ,monokai-gray
                                   :slant normal
                                   :weight normal))))

   `(magit-process-ok
     ((,monokai-class (:inherit magit-section-heading
                                :foreground ,monokai-green))))

   `(magit-process-ng
     ((,monokai-class (:inherit magit-section-heading
                                :foreground ,monokai-red))))

   `(magit-reflog-commit
     ((,monokai-class (:foreground ,monokai-green))))

   `(magit-reflog-amend
     ((,monokai-class (:foreground ,monokai-magenta))))

   `(magit-reflog-merge
     ((,monokai-class (:foreground ,monokai-green))))

   `(magit-reflog-checkout
     ((,monokai-class (:foreground ,monokai-blue))))

   `(magit-reflog-reset
     ((,monokai-class (:foreground ,monokai-red))))

   `(magit-reflog-rebase
     ((,monokai-class (:foreground ,monokai-violet))))

   `(magit-reflog-cherry-pick
     ((,monokai-class (:foreground ,monokai-green))))

   `(magit-reflog-remote
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(magit-reflog-other
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(magit-section-highlight
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(magit-section-heading
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(magit-section-heading-selection
     ((,monokai-class (:foreground ,monokai-orange))))

   `(magit-sequence-stop
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(magit-sequence-part
     ((,monokai-class (:foreground ,monokai-orange))))

   `(magit-sequence-head
     ((,monokai-class (:foreground ,monokai-blue))))

   `(magit-sequence-drop
     ((,monokai-class (:foreground ,monokai-red))))

   `(magit-dimmed
     ((,monokai-class (:foreground ,monokai-comments))))

   `(magit-hash
     ((,monokai-class (:foreground ,monokai-comments))))

   `(magit-tag
     ((,monokai-class (:foreground ,monokai-orange))))

   `(magit-branch-remote
     ((,monokai-class (:foreground ,monokai-green))))

   `(magit-branch-local
     ((,monokai-class (:foreground ,monokai-blue))))

   `(magit-refname
     ((,monokai-class (:foreground ,monokai-comments))))

   `(magit-signature-good
     ((,monokai-class (:foreground ,monokai-green-d))))

   `(magit-signature-bad
     ((,monokai-class (:foreground ,monokai-red-d))))

   `(magit-signature-untrusted
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(magit-signature-expired
     ((,monokai-class (:foreground ,monokai-orange))))

   `(magit-signature-revoked
     ((,monokai-class (:foreground ,monokai-magenta))))

   `(magit-signature-error
     ((,monokai-class (:foreground ,monokai-red-l))))

   `(magit-cherry-unmatched
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(magit-cherry-equivalent
     ((,monokai-class (:foreground ,monokai-magenta))))

   ;; man
   `(Man-overstrike
     ((,monokai-class (:foreground ,monokai-blue))))

   `(Man-reverse
     ((,monokai-class (:foreground ,monokai-orange))))

   `(Man-underline
     ((,monokai-class (:foreground ,monokai-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(monky-diff-add
     ((,monokai-class (:foreground ,monokai-green))))

   `(monky-diff-del
     ((,monokai-class (:foreground ,monokai-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(markdown-header-face-1
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-4))))

   `(markdown-header-face-2
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-3))))

   `(markdown-header-face-3
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-2))))

   `(markdown-header-face-4
     ((,monokai-class (:inherit markdown-header-face
                                :height ,monokai-height-plus-1))))

   `(markdown-header-face-5
     ((,monokai-class (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,monokai-class (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,monokai-class (:foreground ,monokai-comments))))

   `(message-header-name
     ((,monokai-class (:foreground ,monokai-comments))))

   `(message-header-other
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight normal))))

   `(message-header-to
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight normal))))

   `(message-header-cc
     ((,monokai-class (:foreground ,monokai-foreground
                                   :weight normal))))

   `(message-header-newsgroups
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(message-header-subject
     ((,monokai-class (:foreground ,monokai-cyan-l
                                   :weight normal))))

   `(message-header-xheader
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(message-mml
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(message-separator
     ((,monokai-class (:foreground ,monokai-comments
                                   :slant italic))))

   ;; mingus
   `(mingus-directory-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(mingus-pausing-face
     ((,monokai-class (:foreground ,monokai-magenta))))

   `(mingus-playing-face
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(mingus-playlist-face
     ((,monokai-class (:foreground ,monokai-cyan-l ))))

   `(mingus-song-file-face
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(mingus-stopped-face
     ((,monokai-class (:foreground ,monokai-red))))

   ;; moccur
   `(moccur-current-line-face
     ((,monokai-class (:underline t))))

   `(moccur-edit-done-face
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background
                                   :slant italic))))

   `(moccur-edit-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background))))

   `(moccur-edit-file-face
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(moccur-edit-reject-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(moccur-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis))))

   `(search-buffers-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-emphasis))))

   `(search-buffers-header-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-yellow))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,monokai-class (:background ,monokai-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(nav-face-button-num
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(nav-face-dir
     ((,monokai-class (:foreground ,monokai-green))))

   `(nav-face-hdir
     ((,monokai-class (:foreground ,monokai-red))))

   `(nav-face-file
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(nav-face-hfile
     ((,monokai-class (:foreground ,monokai-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,monokai-class (:background ,monokai-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-background))))


   `(neo-header-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))))

   `(neo-root-dir-face
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))))

   `(neo-dir-link-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(neo-file-link-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(neo-button-face
     ((,monokai-class (:underline nil))))

   `(neo-expand-btn-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(neo-vc-default-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(neo-vc-user-face
     ((,monokai-class (:foreground ,monokai-red
                                   :slant italic))))

   `(neo-vc-up-to-date-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(neo-vc-edited-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(neo-vc-needs-update-face
     ((,monokai-class (:underline t))))

   `(neo-vc-needs-merge-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(neo-vc-unlocked-changes-face
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-comments))))

   `(neo-vc-added-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(neo-vc-removed-face
     ((,monokai-class (:strike-through t))))

   `(neo-vc-conflict-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(neo-vc-missing-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(neo-vc-ignored-face
     ((,monokai-class (:foreground ,monokai-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,monokai-class (:foreground ,monokai-gray-l))))

   `(markup-table-face
     ((,monokai-class (:foreground ,monokai-blue-hc
                                   :background ,monokai-blue-lc))))

   `(markup-verbatim-face
     ((,monokai-class (:background ,monokai-orange-lc))))

   `(markup-list-face
     ((,monokai-class (:foreground ,monokai-violet-hc
                                   :background ,monokai-violet-lc))))

   `(markup-replacement-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(markup-complex-replacement-face
     ((,monokai-class (:foreground ,monokai-violet-hc
                                   :background ,monokai-violet-lc))))

   `(markup-gen-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(markup-secondary-text-face
     ((,monokai-class (:foreground ,monokai-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-highlight-line
                                   :slant normal
                                   :inverse-video nil
                                   :height ,monokai-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,monokai-background)))))

   `(org-agenda-calendar-event
     ((,monokai-class (:foreground ,monokai-emphasis))))

   `(org-agenda-calendar-sexp
     ((,monokai-class (:foreground ,monokai-foreground
                                   :slant italic))))

   `(org-agenda-date
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,monokai-background)))) t)

   `(org-agenda-date-weekend
     ((,monokai-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,monokai-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))) t)

   `(org-agenda-date-today
     ((,monokai-class (:inherit org-agenda-date
                                :inverse-video t
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,monokai-blue
                                :background ,monokai-background))) t)

   `(org-agenda-done
     ((,monokai-class (:foreground ,monokai-comments
                                   :slant italic))) t)

   `(org-archived
     ((,monokai-class (:foreground ,monokai-comments
                                   :weight normal))))

   `(org-block
     ((,monokai-class (:foreground ,monokai-violet
                                   :background ,monokai-highlight-alt))))

   `(org-block-background
     ((,monokai-class (:background ,monokai-highlight-alt))))

   `(org-block-begin-line
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-gray-d
                                   :slant italic))))

   `(org-block-end-line
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-gray-d
                                   :slant italic))))

   `(org-checkbox
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground
                                   :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,monokai-class (:foreground ,monokai-orange))))

   `(org-date
     ((,monokai-class (:foreground ,monokai-blue
                                   :underline t))))

   `(org-done
     ((,monokai-class (:foreground ,monokai-green))))

   `(org-ellipsis
     ((,monokai-class (:foreground ,monokai-comments))))

   `(org-formula
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(org-headline-done
     ((,monokai-class (:foreground ,monokai-green))))

   `(org-hide
     ((,monokai-class (:foreground ,monokai-background))))

   `(org-level-1
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-orange))))

   `(org-level-2
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-green))))

   `(org-level-3
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue))))

   `(org-level-4
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-yellow))))

   `(org-level-5
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-cyan-l))))

   `(org-level-6
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-green))))

   `(org-level-7
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-red))))

   `(org-level-8
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue))))

   `(org-link
     ((,monokai-class (:foreground ,monokai-magenta-l
                                   :underline t))))

   `(org-list-dt ((t (:bold nil
                            :foreground ,monokai-red))))

   `(org-sexp-date
     ((,monokai-class (:foreground ,monokai-violet))))

   `(org-scheduled
     ((,monokai-class (:foreground ,monokai-green))))

   `(org-scheduled-previously
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(org-scheduled-today
     ((,monokai-class (:foreground ,monokai-blue
                                   :weight normal))))

   `(org-special-keyword
     ((,monokai-class (:foreground ,monokai-comments))))

   `(org-table
     ((,monokai-class (:foreground ,monokai-green))))

   `(org-tag
     ((,monokai-class (:weight bold))))

   `(org-time-grid
     ((,monokai-class (:foreground ,monokai-comments))))

   `(org-todo
     ((,monokai-class (:foreground ,monokai-red))))

   `(org-upcoming-deadline
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal
                                   :underline nil))))

   `(org-warning
     ((,monokai-class (:foreground ,monokai-orange
                                   :weight normal
                                   :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,monokai-class (:background ,monokai-blue-lc
                                   :foreground ,monokai-blue-hc))))

   `(org-habit-clear-future-face
     ((,monokai-class (:background ,monokai-blue-lc))))

   `(org-habit-ready-face
     ((,monokai-class (:background ,monokai-green-lc
                                   :foreground ,monokai-green))))

   `(org-habit-ready-future-face
     ((,monokai-class (:background ,monokai-green-lc))))

   `(org-habit-alert-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-yellow-lc))))

   `(org-habit-alert-future-face
     ((,monokai-class (:background ,monokai-yellow-lc))))

   `(org-habit-overdue-face
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-red-lc))))

   `(org-habit-overdue-future-face
     ((,monokai-class (:background ,monokai-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(org-agenda-restriction-lock
     ((,monokai-class (:background ,monokai-yellow))))

   `(org-clock-overlay
     ((,monokai-class (:background ,monokai-yellow))))

   `(org-column
     ((,monokai-class (:background ,monokai-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))))

   `(org-column-title
     ((,monokai-class (:background ,monokai-highlight-line
                                   :underline t))))

   `(org-date-selected
     ((,monokai-class (:foreground ,monokai-red
                                   :inverse-video t))))

   `(org-document-info
     ((,monokai-class (:foreground ,monokai-blue))))

   `(org-document-title
     ((,monokai-class (:foreground ,monokai-blue))))

   `(org-drawer
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(org-footnote
     ((,monokai-class (:foreground ,monokai-orange-lc
                                   :underline t))))

   `(org-latex-and-export-specials
     ((,monokai-class (:foreground ,monokai-orange))))

   `(org-mode-line-clock-overrun
     ((,monokai-class (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,monokai-class (:inherit org-level-1))))

   `(outline-2
     ((,monokai-class (:inherit org-level-2))))

   `(outline-3
     ((,monokai-class (:inherit org-level-3))))

   `(outline-4
     ((,monokai-class (:inherit org-level-4))))

   `(outline-5
     ((,monokai-class (:inherit org-level-5))))

   `(outline-6
     ((,monokai-class (:inherit org-level-6))))

   `(outline-7
     ((,monokai-class (:inherit org-level-7))))

   `(outline-8
     ((,monokai-class (:inherit org-level-8))))

   ;; parenface
   `(paren-face ())

   ;; perspective
   `(persp-selected-face
     ((,monokai-class (:foreground ,monokai-blue))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :weight normal))))

   ;; popup
   `(popup-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))))

   `(popup-isearch-match
     ((,monokai-class (:background ,monokai-green))))

   `(popup-menu-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))))

   `(popup-menu-mouse-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-foreground))))

   `(popup-menu-selection-face
     ((,monokai-class (:background ,monokai-magenta
                                   :foreground ,monokai-background))))

   `(popup-scroll-bar-background-face
     ((,monokai-class (:background ,monokai-comments))))

   `(popup-scroll-bar-foreground-face
     ((,monokai-class (:background ,monokai-emphasis))))

   `(popup-tip-face
     ((,monokai-class (:background ,monokai-highlight-line
                                   :foreground ,monokai-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(rainbow-delimiters-depth-4-face
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(rainbow-delimiters-depth-7-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(rainbow-delimiters-depth-10-face
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(rainbow-delimiters-unmatched-face
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-background
                                   :inverse-video t))))

   ;; realgud
   `(realgud-overlay-arrow1
     ((,monokai-class (:foreground ,monokai-green-d))))

   `(realgud-overlay-arrow2
     ((,monokai-class (:foreground ,monokai-yellow-d))))

   `(realgud-overlay-arrow3
     ((,monokai-class (:foreground ,monokai-orange-d))))

   `(realgud-bp-enabled-face
     ((,monokai-class (:inherit error))))

   `(realgud-bp-disabled-face
     ((,monokai-class (:inherit secondary-selection))))

   `(realgud-bp-line-enabled-face
     ((,monokai-class (:foreground ,monokai-red-d))))

   `(realgud-bp-line-disabled-face
     ((,monokai-class (:inherit secondary-selection))))

   `(realgud-line-number
     ((,monokai-class (:inerhit monokai-line-number))))

   `(realgud-backtrace-number
     ((,monokai-class (:foreground ,monokai-yellow-d))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,monokai-class (:foreground ,monokai-violet))))

   `(sh-escaped-newline
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(sh-heredoc
     ((,monokai-class (:foreground ,monokai-yellow))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(sp-wrap-overlay-face
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,monokai-class (:background ,monokai-highlight-line))))

   `(sp-show-pair-enclosing
     ((,monokai-class (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))))

   `(show-paren-mismatch
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))))

   `(paren-face-mismatch
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))))

   `(paren-face-no-match
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background
                                   :weight normal
                                   :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,monokai-class (:foreground ,monokai-red))))

   ;; smerge
   `(smerge-base
     ((,monokai-class (:background ,monokai-diff-blue-base))))
   `(smerge-upper
     ((,monokai-class (:background ,monokai-diff-red-base))))
   `(smerge-lower
     ((,monokai-class (:background ,monokai-diff-green-base))))
   ;; WARNING: defining this face will overwrite the next two when displaying a
   ;; smerge diff in a file.
   ;; `(smerge-refined-changed
   ;;    ((,monokai-class (:background ,monokai-diff-blue-emphasis)) ;;      ))
   `(smerge-refined-added
     ((,monokai-class (:background ,monokai-diff-green-emphasis))))
   `(smerge-refined-removed
     ((,monokai-class (:background ,monokai-diff-red-emphasis))))

   ;; speedbar
   `(speedbar-button-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-comments))))

   `(speedbar-directory-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-blue))))

   `(speedbar-file-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-foreground))))

   `(speedbar-highlight-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :background ,monokai-highlight-line))))

   `(speedbar-selected-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-yellow
                                :underline t))))

   `(speedbar-separator-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :background ,monokai-blue
                                :foreground ,monokai-background
                                :overline ,monokai-cyan-lc))))

   `(speedbar-tag-face
     ((,monokai-class (:inherit ,monokai-pitch
                                :foreground ,monokai-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,monokai-class (:background ,monokai-blue
                                   :foreground ,monokai-background
                                   :height ,monokai-height-plus-1))))

   `(sr-editing-path-face
     ((,monokai-class (:background ,monokai-yellow
                                   :foreground ,monokai-background
                                   :height ,monokai-height-plus-1))))

   `(sr-highlight-path-face
     ((,monokai-class (:background ,monokai-green
                                   :foreground ,monokai-background
                                   :height ,monokai-height-plus-1))))

   `(sr-passive-path-face
     ((,monokai-class (:background ,monokai-comments
                                   :foreground ,monokai-background
                                   :height ,monokai-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,monokai-class (:inherit dimonokai-red-marked))))

   `(sr-marked-file-face
     ((,monokai-class (:inherit dimonokai-red-marked))))

   `(sr-alt-marked-dir-face
     ((,monokai-class (:background ,monokai-magenta
                                   :foreground ,monokai-background))))

   `(sr-alt-marked-file-face
     ((,monokai-class (:background ,monokai-magenta
                                   :foreground ,monokai-background))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,monokai-class (:inherit dimonokai-red-directory
                                :weight normal))))

   `(sr-symlink-directory-face
     ((,monokai-class (:inherit dimonokai-red-directory
                                :slant italic
                                :weight normal))))

   `(sr-symlink-face
     ((,monokai-class (:inherit dimonokai-red-symlink
                                :slant italic
                                :weight normal))))

   `(sr-broken-link-face
     ((,monokai-class (:inherit dimonokai-red-warning
                                :slant italic
                                :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(sr-encrypted-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(sr-log-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(sr-packaged-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(sr-html-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(sr-xml-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,monokai-class (:background ,monokai-red
                                   :foreground ,monokai-background))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-yellow))))

   `(syslog-hour-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-green))))

   `(syslog-error-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-red))))

   `(syslog-warn-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-orange))))

   `(syslog-info-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-blue))))

   `(syslog-debug-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-cyan-l))))

   `(syslog-su-face
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-magenta))))

   ;; table
   `(table-cell
     ((,monokai-class (:foreground ,monokai-foreground
                                   :background ,monokai-highlight-line))))

   ;; term
   `(term-color-black
     ((,monokai-class (:foreground ,monokai-background
                                   :background ,monokai-highlight-line))))

   `(term-color-red
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-red-d))))

   `(term-color-green
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-green-d))))

   `(term-color-yellow
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-yellow-d))))

   `(term-color-blue
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-blue-d))))

   `(term-color-magenta
     ((,monokai-class (:foreground ,monokai-magenta
                                   :background ,monokai-magenta-d))))

   `(term-color-cyan
     ((,monokai-class (:foreground ,monokai-cyan-l
                                   :background ,monokai-cyan-l))))

   `(term-color-white
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-foreground))))

   `(term-default-fg-color
     ((,monokai-class (:inherit term-color-white))))

   `(term-default-bg-color
     ((,monokai-class (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,monokai-class (:background ,monokai-background
                                   :foreground ,monokai-foreground
                                   :inherit ,monokai-pitch))))

   ;; treemacs
   `(treemacs-directory-face
     ((,monokai-class (:foreground ,monokai-violet
                                   :background ,monokai-background))))

   `(treemacs-header-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-background
                                   :underline t))))

   `(treemacs-git-modified-face
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))))

   `(treemacs-git-renamed-face
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background))))

   `(treemacs-git-ignored-face
     ((,monokai-class (:foreground ,monokai-gray-l
                                   :background ,monokai-background))))

   `(treemacs-git-untracked-face
     ((,monokai-class (:foreground ,monokai-red
                                   :background ,monokai-background))))

   `(treemacs-git-added-face
     ((,monokai-class (:foreground ,monokai-green
                                   :background ,monokai-background))))

   `(treemacs-git-conflict-face
     ((,monokai-class (:foreground ,monokai-orange
                                   :background ,monokai-background))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,monokai-class (:foreground ,monokai-magenta))))

   `(tuareg-font-lock-multistage-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :background ,monokai-highlight-line))))

   `(tuareg-font-lock-operator-face
     ((,monokai-class (:foreground ,monokai-emphasis))))

   `(tuareg-font-lock-error-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :background ,monokai-red))))

   `(tuareg-font-lock-interactive-output-face
     ((,monokai-class (:foreground ,monokai-cyan-l))))

   `(tuareg-font-lock-interactive-error-face
     ((,monokai-class (:foreground ,monokai-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(undo-tree-visualizer-current-face
     ((,monokai-class (:foreground ,monokai-blue
                                   :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,monokai-class (:foreground ,monokai-emphasis
                                   :background ,monokai-background))))

   `(undo-tree-visualizer-register-face
     ((,monokai-class (:foreground ,monokai-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,monokai-class (:background ,monokai-highlight-alt))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(web-mode-comment-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(web-mode-constant-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(web-mode-current-element-highlight-face
     ((,monokai-class (:underline unspecified
                                  :weight unspecified
                                  :background ,monokai-highlight-line))))

   `(web-mode-doctype-face
     ((,monokai-class (:foreground ,monokai-comments
                                   :slant italic
                                   :weight bold))))

   `(web-mode-folded-face
     ((,monokai-class (:underline t))))

   `(web-mode-function-name-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(web-mode-html-attr-name-face
     ((,monokai-class (:foreground ,monokai-blue))))

   `(web-mode-html-attr-custom-face
     ((,monokai-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,monokai-class (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,monokai-class (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(web-mode-html-tag-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(web-mode-html-tag-bracket-face
     ((,monokai-class (:foreground ,monokai-gray))))

   `(web-mode-keyword-face
     ((,monokai-class (:foreground ,monokai-red))))

   `(web-mode-preprocessor-face
     ((,monokai-class (:foreground ,monokai-yellow
                                   :slant normal
                                   :weight unspecified))))

   `(web-mode-string-face
     ((,monokai-class (:foreground ,monokai-yellow))))

   `(web-mode-type-face
     ((,monokai-class (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,monokai-class (:foreground ,monokai-orange))))

   `(web-mode-warning-face
     ((,monokai-class (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,monokai-class (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,monokai-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,monokai-class (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,monokai-class (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,monokai-class (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,monokai-class (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,monokai-class (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,monokai-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,monokai-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,monokai-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,monokai-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,monokai-class (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,monokai-class (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,monokai-class (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,monokai-class (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,monokai-class (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,monokai-class (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,monokai-class (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(web-mode-json-key-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(web-mode-json-string-face
     ((,monokai-class (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(web-mode-part-comment-face
     ((,monokai-class (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,monokai-class (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,monokai-class (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,monokai-class (:foreground ,monokai-violet))))

   `(web-mode-whitespace-face
     ((,monokai-class (:background ,monokai-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-comments
                                   :inverse-video unspecified
                                   :slant italic))))

   `(whitespace-hspace
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-emphasis
                                   :inverse-video unspecified))))

   `(whitespace-tab
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-red
                                   :inverse-video unspecified))))

   `(whitespace-newline
     ((,monokai-class(:background unspecified
                                  :foreground ,monokai-comments
                                  :inverse-video unspecified))))

   `(whitespace-trailing
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-orange-lc
                                   :inverse-video t))))

   `(whitespace-line
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-magenta
                                   :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,monokai-class (:background ,monokai-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))))

   `(whitespace-indentation
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-yellow
                                   :inverse-video unspecified))))

   `(whitespace-empty
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-red-lc
                                   :inverse-video t))))

   `(whitespace-space-after-tab
     ((,monokai-class (:background unspecified
                                   :foreground ,monokai-orange
                                   :inverse-video t))))

   ;; which-func-mode
   `(which-func
     ((,monokai-class (:foreground ,monokai-green))))

   ;; which-key
   `(which-key-key-face
     ((,monokai-class (:foreground ,monokai-green))))

   `(which-key-separator-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(which-key-note-face
     ((,monokai-class (:foreground ,monokai-comments))))

   `(which-key-command-description-face
     ((,monokai-class (:foreground ,monokai-foreground))))

   `(which-key-local-map-description-face
     ((,monokai-class (:foreground ,monokai-yellow-hc))))

   `(which-key-group-description-face
     ((,monokai-class (:foreground ,monokai-red))))

   ;; window-divider-mode
   `(window-divider
     ((,monokai-class (:foreground ,monokai-highlight))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel ((t (:inherit window-divider))))

   ;; window-number-mode
   `(window-number-face
     ((,monokai-class (:foreground ,monokai-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-comments))))

   `(yascroll:thumb-fringe
     ((,monokai-class (:foreground ,monokai-comments
                                   :background ,monokai-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,monokai-class (:background ,monokai-highlight-line
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
