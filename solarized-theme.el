;;; solarized-theme.el --- a light variant of Solarized -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: January 9, 2018 (#131)
;; Author: fuxialexander <https://github.com/fuxialexander>
;; Maintainer:
;; Source: https://github.com/bbatsov/solarized-emacs
;; Source: https://ethanschoonover.com/solarized
;;

;;; Commentary:
;;
;; ## Install
;;
;;   `M-x package-install RET doom-themes`
;;
;; A comprehensive configuration example:
;;
;;   (require 'doom-themes)
;;
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;
;;   ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
;;   ;; theme may have their own settings.
;;   (load-theme 'doom-one t)
;;
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;
;;   ;; Enable custom neotree theme
;;   (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;
;;; Code:

(require 'cl-lib)

;; These are used as a basis for every Doom theme defined with `def-doom-theme',
;; as a set of reasonble defaults. They are intended to be overidden where it
;; makes sense to.
(defvar doom-themes-base-faces
  '((bold        :weight 'bold :foreground (if bold 'unspecified base8))
    (bold-italic :inherit '(bold italic))
    (italic      :slant  'italic)
    (escape-glyph :foreground cyan)
    (default :background bg :foreground fg)
    (fringe  :inherit 'default :foreground base4)
    (region               :background region :distant-foreground (doom-darken fg 0.2) :extend t)
    (highlight            :background highlight :foreground base0 :distant-foreground base8)
    (cursor               :background highlight)
    (shadow               :foreground base5)
    (minibuffer-prompt    :foreground highlight)
    (tooltip              :background bg-alt :foreground fg)
    (secondary-selection  :background grey :extend t)
    (lazy-highlight
     (&dark  :background (doom-darken highlight 0.3)   :foreground base8 :distant-foreground base0 :weight 'bold)
     (&light :background (doom-blend bg highlight 0.7) :foreground base0 :distant-foreground base8))
    (match                :foreground green      :background base0 :weight 'bold)
    (trailing-whitespace  :background red)
    (nobreak-space        :inherit 'escape-glyph :underline t)
    (vertical-border      :background vertical-bar :foreground vertical-bar)
    (link                 :foreground highlight :underline t :weight 'bold)
    (error   :foreground error)
    (warning :foreground warning)
    (success :foreground success)
    ;;;; font-lock-* faces
    (font-lock-builtin-face              :foreground builtin)
    (font-lock-comment-face              :foreground comments)
    (font-lock-comment-delimiter-face    :inherit 'font-lock-comment-face)
    (font-lock-doc-face                  :inherit 'font-lock-comment-face :foreground doc-comments)
    (font-lock-constant-face             :foreground constants)
    (font-lock-function-name-face        :foreground functions)
    (font-lock-keyword-face              :foreground keywords)
    (font-lock-string-face               :foreground strings)
    (font-lock-type-face                 :foreground type)
    (font-lock-variable-name-face        :foreground variables)
    (font-lock-number-face               :foreground numbers)
    (font-lock-warning-face              :inherit 'warning)
    (font-lock-negation-char-face        :inherit 'bold :foreground operators)
    (font-lock-preprocessor-face         :inherit 'bold :foreground operators)
    (font-lock-preprocessor-char-face    :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-backslash :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-construct :inherit 'bold :foreground operators)
    ;;;; mode-line / header-line
    (mode-line           :background bg     :foreground fg     :distant-foreground bg)
    (mode-line-active    :inherit 'mode-line)
    (mode-line-inactive  :background bg-alt :foreground fg-alt :distant-foreground bg-alt)
    (mode-line-emphasis  :foreground highlight :distant-foreground bg)
    (mode-line-highlight :inherit 'highlight :distant-foreground bg)
    (mode-line-buffer-id :weight 'bold)
    (header-line :inherit 'mode-line)
    (header-line-highlight :inherit 'mode-line-highlight)
    ;;;; tab-line/tab-bar (Emacs 27+)
    (tab-line :background bg-alt :foreground bg-alt)
    (tab-line-tab :background bg :foreground fg)
    (tab-line-tab-inactive :inherit 'tab-line-tab :background bg-alt :foreground fg-alt)
    (tab-line-tab-inactive-alternate :inherit 'tab-line-tab-inactive)
    (tab-line-tab-current :background bg :foreground fg)
    ;; (tab-line-special )
    (tab-line-highlight :inherit 'tab-line-tab)
    (tab-line-close-highlight :foreground highlight)
    ((tab-bar &inherit tab-line))
    ((tab-bar-tab &inherit tab-line-tab))
    ((tab-bar-tab-inactive &inherit tab-line-tab-inactive))
    ;;;; Line numbers
    ;; 1. Line number faces must explicitly disable its text style attributes
    ;;    because nearby faces may "bleed" into the line numbers otherwise.
    ;; 2. All other line number plugin faces should &inherit from these.
    (line-number
     :inherit 'default
     :foreground base5 :distant-foreground 'unspecified
     :weight 'normal :italic 'unspecified
     :underline 'unspecified :strike-through 'unspecified)
    (line-number-current-line
     :inherit '(hl-line default)
     :foreground fg :distant-foreground 'unspecified
     :weight 'normal :italic 'unspecified
     :underline 'unspecified :strike-through 'unspecified)

    ;;;; --- Package faces ----------------------
    ;; What follows are faces for all the packages doom-themes explicitly
    ;; supports. Headings are formatted as such:
    ;;
    ;;   PACKAGE [<built-in>] [<modes:some-mode[, ...]>]
    ;;
    ;; The purpose of this is to make it easy to jump to via `imenu', or search
    ;; for with isearch, swiper, etc.
    ;;;; auctex <modes:latex-mode>
    (font-latex-bold-face         :inherit 'bold)
    (font-latex-italic-face       :inherit 'italic)
    (font-latex-math-face         :foreground blue)
    (font-latex-sedate-face       :inherit 'font-lock-keyword-face)
    (font-latex-sectioning-0-face :foreground blue    :weight 'ultra-bold)
    (font-latex-sectioning-1-face :foreground magenta :weight 'semi-bold)
    (font-latex-sectioning-2-face :foreground violet  :weight 'semi-bold)
    (font-latex-sectioning-3-face :foreground (doom-lighten blue 0.3)    :weight 'semi-bold)
    (font-latex-sectioning-4-face :foreground (doom-lighten magenta 0.3) :weight 'semi-bold)
    (font-latex-sectioning-5-face :foreground (doom-lighten violet 0.3)  :weight 'semi-bold)
    (font-latex-script-char-face  :foreground dark-blue)
    (font-latex-string-face       :inherit 'font-lock-string-face)
    (font-latex-warning-face      :inherit 'font-lock-warning-face)
    (font-latex-verbatim-face     :inherit 'fixed-pitch :foreground violet :slant 'italic)
    (TeX-error-description-error    :inherit 'error   :weight 'bold)
    (TeX-error-description-warning  :inherit 'warning :weight 'bold)
    (TeX-error-description-tex-said :inherit 'success :weight 'bold)
    ;;;; alert
    (alert-high-face         :inherit bold :foreground warning)
    (alert-low-face          :foreground grey)
    (alert-moderate-face     :inherit bold :foreground fg-alt)
    (alert-trivial-face      :foreground doc-comments)
    (alert-urgent-face       :inherit bold :foreground error)
    ;;;; all-the-icons
    (all-the-icons-blue       :foreground blue)
    (all-the-icons-blue-alt   :foreground teal)
    (all-the-icons-cyan       :foreground cyan)
    (all-the-icons-cyan-alt   :foreground cyan)
    (all-the-icons-dblue      :foreground dark-blue)
    (all-the-icons-dcyan      :foreground dark-cyan)
    (all-the-icons-dgreen     :foreground (doom-darken green 0.3))
    (all-the-icons-dmaroon    :foreground (doom-darken magenta 0.3))
    (all-the-icons-dorange    :foreground (doom-darken orange 0.3))
    (all-the-icons-dpink      :foreground (doom-lighten red 0.15))
    (all-the-icons-dpurple    :foreground (doom-darken violet 0.3))
    (all-the-icons-dred       :foreground (doom-darken red 0.3))
    (all-the-icons-dsilver    :foreground (doom-lighten grey 0.1))
    (all-the-icons-dyellow    :foreground (doom-darken yellow 0.3))
    (all-the-icons-green      :foreground green)
    (all-the-icons-lblue      :foreground (doom-lighten blue 0.3))
    (all-the-icons-lcyan      :foreground (doom-lighten cyan 0.3))
    (all-the-icons-lgreen     :foreground (doom-lighten green 0.3))
    (all-the-icons-lmaroon    :foreground (doom-lighten magenta 0.3))
    (all-the-icons-lorange    :foreground (doom-lighten orange 0.3))
    (all-the-icons-lpink      :foreground (doom-lighten red 0.55))
    (all-the-icons-lpurple    :foreground (doom-lighten violet 0.3))
    (all-the-icons-lred       :foreground (doom-lighten red 0.3))
    (all-the-icons-lsilver    :foreground (doom-lighten grey 0.7))
    (all-the-icons-lyellow    :foreground (doom-lighten yellow 0.3))
    (all-the-icons-maroon     :foreground magenta)
    (all-the-icons-orange     :foreground orange)
    (all-the-icons-pink       :foreground (doom-lighten red 0.35))
    (all-the-icons-purple     :foreground violet)
    (all-the-icons-purple-alt :foreground (doom-blend violet grey 0.15))
    (all-the-icons-red        :foreground red)
    (all-the-icons-red-alt    :foreground (doom-blend red grey 0.15))
    (all-the-icons-silver     :foreground (doom-lighten grey 0.45))
    (all-the-icons-yellow     :foreground yellow)
    ;;;; all-the-icons-dired
    (all-the-icons-dired-dir-face    :foreground doc-comments)
    ;;;; annotate
    (annotate-annotation           :background (doom-blend highlight bg 0.1) :foreground doc-comments)
    (annotate-annotation-secondary :background (doom-blend green bg 0.1)     :foreground doc-comments)
    (annotate-highlight            :background (doom-blend highlight bg 0.1) :underline highlight)
    (annotate-highlight-secondary  :background (doom-blend green bg 0.1)     :underline green)
    ;;;; ansi-color <built-in>
    (ansi-color-black          :foreground bg      :background bg)
    (ansi-color-red            :foreground red     :background red)
    (ansi-color-green          :foreground green   :background green)
    (ansi-color-yellow         :foreground yellow  :background yellow)
    (ansi-color-blue           :foreground blue    :background blue)
    (ansi-color-magenta        :foreground magenta :background magenta)
    (ansi-color-cyan           :foreground cyan    :background cyan)
    (ansi-color-white          :foreground fg      :background fg)
    ;; This color is used effectively as grayed out foreground text.
    ;; base5 and up have too much contrast in light themes;
    ;; base5 and lower have too little contrast in dark themes.
    (ansi-color-bright-black
     (&light :foreground base4 :background base4)
     (&dark  :foreground base6 :background base6))
    (ansi-color-bright-red     :foreground (doom-lighten red 0.15)     :background (doom-lighten red 0.15))
    (ansi-color-bright-green   :foreground (doom-lighten green 0.15)   :background (doom-lighten green 0.15))
    (ansi-color-bright-yellow  :foreground (doom-lighten yellow 0.15)  :background (doom-lighten yellow 0.15))
    (ansi-color-bright-blue    :foreground (doom-lighten blue 0.15)    :background (doom-lighten blue 0.15))
    (ansi-color-bright-magenta :foreground (doom-lighten magenta 0.15) :background (doom-lighten magenta 0.15))
    (ansi-color-bright-cyan    :foreground (doom-lighten cyan 0.15)    :background (doom-lighten cyan 0.15))
    (ansi-color-bright-white   :foreground base8   :background base8)
    ;;;; anzu
    (anzu-replace-highlight :background base0 :foreground red   :weight 'bold :strike-through t)
    (anzu-replace-to        :background base0 :foreground green :weight 'bold)
    ;;;; avy
    (avy-background-face :foreground comments)
    (avy-lead-face :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (avy-lead-face-0
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (doom-lighten highlight 0.3))
     (&light :background (doom-darken highlight 0.3)))
    (avy-lead-face-1
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (doom-lighten highlight 0.6))
     (&light :background (doom-darken highlight 0.6)))
    (avy-lead-face-2
     (&all   :inherit 'avy-lead-face)
     (&dark  :background (doom-lighten highlight 0.9))
     (&light :background (doom-darken highlight 0.9)))
    ;;;; bookmark
    (bookmark-face :background (doom-blend highlight bg 0.1) :extend t)
    ;;;; bookmark+
    (bmkp-*-mark :foreground bg :background yellow)
    (bmkp->-mark :foreground yellow)
    (bmkp-D-mark :foreground bg :background red)
    (bmkp-X-mark :foreground red)
    (bmkp-a-mark :background red)
    (bmkp-bad-bookmark :foreground bg :background yellow)
    (bmkp-bookmark-file :foreground violet :background bg-alt)
    (bmkp-bookmark-list :background bg-alt)
    (bmkp-buffer :foreground blue)
    (bmkp-desktop :foreground bg :background violet)
    (bmkp-file-handler :background red)
    (bmkp-function :foreground green)
    (bmkp-gnus :foreground orange)
    (bmkp-heading :foreground yellow)
    (bmkp-info :foreground cyan)
    (bmkp-light-autonamed :foreground bg-alt :background cyan)
    (bmkp-light-autonamed-region :foreground bg-alt :background red)
    (bmkp-light-fringe-autonamed :foreground bg-alt :background violet)
    (bmkp-light-fringe-non-autonamed :foreground bg-alt :background green)
    (bmkp-light-mark :foreground bg :background cyan)
    (bmkp-light-non-autonamed :foreground bg :background violet)
    (bmkp-light-non-autonamed-region :foreground bg :background red)
    (bmkp-local-directory :foreground bg :background violet)
    (bmkp-local-file-with-region :foreground yellow)
    (bmkp-local-file-without-region :foreground comments)
    (bmkp-man :foreground violet)
    (bmkp-no-jump :foreground comments)
    (bmkp-no-local :foreground yellow)
    (bmkp-non-file :foreground green)
    (bmkp-remote-file :foreground orange)
    (bmkp-sequence :foreground blue)
    (bmkp-su-or-sudo :foreground red)
    (bmkp-t-mark :foreground violet)
    (bmkp-url :foreground blue :underline t)
    (bmkp-variable-list :foreground green)
    ;;;; centaur-tabs
    ((centaur-tabs-default &inherit tab-bar) :box nil)
    ((centaur-tabs-selected &inherit tab-bar-tab) :box nil)
    ((centaur-tabs-unselected &inherit tab-bar-tab-inactive) :box nil)
    (centaur-tabs-selected-modified   :background bg :foreground teal)
    (centaur-tabs-unselected-modified :background bg-alt :foreground teal)
    (centaur-tabs-active-bar-face :background highlight)
    (centaur-tabs-modified-marker-selected
     :foreground highlight
     :inherit 'centaur-tabs-selected)
    (centaur-tabs-modified-marker-unselected
     :foreground highlight
     :inherit 'centaur-tabs-unselected)
    ;;;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-common                           :foreground highlight :distant-foreground base0 :weight 'bold)
    (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (company-tooltip-search-selection :background (doom-darken selection 0.25))
    (company-tooltip-selection  :background selection :weight 'bold)
    (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
    (company-tooltip-annotation                       :foreground violet :distant-foreground bg)
    (company-scrollbar-bg       :inherit 'tooltip)
    (company-scrollbar-fg       :background highlight)
    (company-preview                              :foreground comments)
    (company-preview-common     :background base3 :foreground highlight)
    (company-preview-search     :inherit 'company-tooltip-search)
    (company-template-field     :inherit 'match)
    ;;;; company-box
    (company-box-candidate :foreground fg)
    ;;;; corfu
    (corfu-default :inherit 'tooltip)
    (corfu-current :background bg :foreground fg)
    ;;;; circe
    (circe-fool :foreground doc-comments)
    (circe-highlight-nick-face :weight 'bold :foreground constants)
    (circe-prompt-face :weight 'bold :foreground highlight)
    (circe-server-face :foreground comments)
    (circe-my-message-face :weight 'bold)
    ;;;; cperl <built-in>
    (cperl-array-face          :weight 'bold :inherit 'font-lock-variable-name-face)
    (cperl-hash-face           :weight 'bold :slant 'italic :inherit 'font-lock-variable-name-face)
    (cperl-nonoverridable-face :inherit 'font-lock-builtin-face)
    ;;;; compilation <built-in>
    (compilation-column-number  :inherit 'font-lock-comment-face)
    (compilation-line-number    :foreground highlight)
    (compilation-error   :inherit 'error   :weight 'bold)
    (compilation-warning :inherit 'warning :slant 'italic)
    (compilation-info    :inherit 'success)
    (compilation-mode-line-exit :inherit 'compilation-info)
    (compilation-mode-line-fail :inherit 'compilation-error)
    ;;;; custom <built-in>
    (custom-button                  :foreground blue   :background bg     :box '(:line-width 1 :style nil))
    (custom-button-unraised         :foreground violet :background bg     :box '(:line-width 1 :style nil))
    (custom-button-pressed-unraised :foreground bg     :background violet :box '(:line-width 1 :style nil))
    (custom-button-pressed          :foreground bg     :background blue   :box '(:line-width 1 :style nil))
    (custom-button-mouse            :foreground bg     :background blue   :box '(:line-width 1 :style nil))
    (custom-variable-button         :foreground green  :underline t)
    (custom-saved                   :foreground green  :background (doom-blend green bg 0.2) :bold bold)
    (custom-comment                 :foreground fg     :background region)
    (custom-comment-tag             :foreground grey)
    (custom-modified                :foreground blue   :background (doom-blend blue bg 0.2))
    (custom-variable-tag            :foreground magenta)
    (custom-visibility              :foreground blue   :underline 'unspecified)
    (custom-group-subtitle          :foreground red)
    (custom-group-tag               :foreground violet)
    (custom-group-tag-1             :foreground blue)
    (custom-set                     :foreground yellow :background bg)
    (custom-themed                  :foreground yellow :background bg)
    (custom-invalid                 :foreground red    :background (doom-blend red bg 0.2))
    (custom-variable-obsolete       :foreground grey   :background bg)
    (custom-state                   :foreground green  :background (doom-blend green bg 0.2))
    (custom-changed                 :foreground blue   :background bg)
    ;;;; cider
    ;; (cider-stacktrace-error-class-face :inherit 'font-lock-warning-face)
    ;; (cider-stacktrace-error-message-face :inherit 'font-lock-doc-face)
    ;; (cider-stacktrace-filter-active-face :inherit 'button :underline t :weight 'normal)
    ;; (cider-stacktrace-filter-inactive-face :inherit 'cider-stacktrace-filter-active-face :underline nil)
    ;; (cider-stacktrace-face :inherit 'default)
    ;; (cider-stacktrace-ns-face :inherit 'font-lock-comment-face)
    ;; (cider-stacktrace-fn-face :inherit 'default :weight 'bold)
    ;; (cider-docview-emphasis-face :inherit 'default :underline t)
    ;; (cider-docview-strong-face :inherit 'default :underline t :weight 'bold)
    ;; (cider-docview-literal-face :inherit 'font-lock-string-face)
    ;; (cider-docview-table-border-face :inherit 'shadow)
    (cider-debug-code-overlay-face :background base3)
    ;; (cider-debug-prompt-face :inherit font-lock-builtin-face :underline t)
    (cider-enlightened-face
     :inherit 'cider-result-overlay-face :box `(:color ,orange :line-width -1))
    (cider-enlightened-local-face :foreground orange :weight 'bold)
    ;; (cider-repl-prompt-face :inherit 'font-lock-keyword-face)
    ;; (cider-repl-stdout-face :inherit 'font-lock-string-face)
    ;; (cider-repl-stderr-face :inherit 'font-lock-warning-face)
    ;; (cider-repl-input-face :weight 'bold)
    ;; (cider-repl-result-face )
    (cider-result-overlay-face :background base3 :box `(:line-width -1 :color ,base5))
    (cider-fringe-good-face    :foreground green)
    (cider-deprecated-face     :background (doom-blend bg yellow 0.8))
    (cider-instrumented-face   :background (doom-blend bg red 0.8))
    (cider-traced-face         :background (doom-blend bg cyan 0.8))
    ;; (cider-reader-conditional-face :inherit 'font-lock-comment-face)
    (cider-error-highlight-face
     `((((supports :underline (:style wave)))
        (:inherit unspecified :underline (:style wave :color ,(car error))))
       (t (:inherit font-lock-warning-face :underline t))))
    (cider-warning-highlight-face
     `((((supports :underline (:style wave)))
        (:underline (:style wave :color ,(car warning)) :inherit unspecified))
       (t (:inherit font-lock-warning-face :underline (:color ,(car warning))))))
    (cider-test-failure-face :background (doom-blend bg error 0.7))
    (cider-test-error-face   :background orange)
    (cider-test-success-face
     (&light :foreground base0 :background green)
     (&dark  :foreground green :background base0))
    ;;;; diff-hl
    (diff-hl-change :foreground vc-modified :background vc-modified)
    (diff-hl-delete :foreground vc-deleted :background vc-deleted)
    (diff-hl-insert :foreground vc-added :background vc-added)
    ;;;; diff-mode <built-in>
    (diff-added   :inherit 'hl-line :foreground green)
    (diff-changed :foreground violet)
    (diff-context
     (&dark  :foreground (doom-darken fg 0.12))
     (&light :foreground (doom-lighten fg 0.12)))
    (diff-removed :foreground red :background base3)
    (diff-header  :foreground cyan)
    (diff-file-header :foreground blue)
    (diff-hunk-header :foreground violet)
    (diff-refine-added   :inherit 'diff-added :inverse-video t)
    (diff-refine-changed :inherit 'diff-changed :inverse-video t)
    (diff-refine-removed :inherit 'diff-removed :inverse-video t)
    ;;;; dired <built-in>
    (dired-directory  :foreground builtin)
    (dired-ignored    :foreground comments)
    (dired-flagged    :foreground red)
    (dired-header     :foreground blue :weight 'bold)
    (dired-mark       :foreground orange :weight 'bold)
    (dired-marked     :foreground magenta :weight 'bold :inverse-video t)
    (dired-perm-write :foreground fg :underline t)
    (dired-symlink    :foreground cyan :weight 'bold)
    (dired-warning    :foreground warning)
    ;;;; dired+
    (diredp-file-name              :foreground base8)
    (diredp-dir-name               :foreground base8 :weight 'bold)
    (diredp-ignored-file-name      :foreground base5)
    (diredp-compressed-file-suffix :foreground base5)
    (diredp-symlink                :foreground violet)
    (diredp-dir-heading            :foreground blue  :weight 'bold)
    (diredp-file-suffix            :foreground violet)
    (diredp-read-priv              :foreground magenta)
    (diredp-write-priv             :foreground green)
    (diredp-exec-priv              :foreground yellow)
    (diredp-rare-priv              :foreground red   :weight 'bold)
    (diredp-dir-priv               :foreground blue  :weight 'bold)
    (diredp-no-priv                :foreground base5)
    (diredp-number                 :foreground magenta)
    (diredp-date-time              :foreground blue)
    ;;;; dired-k
    (dired-k-modified :foreground vc-modified :weight 'bold)
    (dired-k-commited :foreground green :weight 'bold)
    (dired-k-added :foreground vc-added :weight 'bold)
    (dired-k-untracked :foreground teal :weight 'bold)
    (dired-k-ignored :foreground base5 :weight 'bold)
    (dired-k-directory :foreground blue :weight 'bold)
    ;;;; dired-subtree
    (dired-subtree-depth-1-face :background (doom-darken bg-alt 0.02))
    (dired-subtree-depth-2-face :background (doom-darken bg-alt 0.04))
    (dired-subtree-depth-3-face :background (doom-darken bg-alt 0.06))
    (dired-subtree-depth-4-face :background (doom-darken bg-alt 0.08))
    (dired-subtree-depth-5-face :background (doom-darken bg-alt 0.10))
    (dired-subtree-depth-6-face :background (doom-darken bg-alt 0.12))
    ;;;; diredfl
    (diredfl-autofile-name          :foreground base4)
    (diredfl-compressed-file-name   :foreground yellow)
    (diredfl-compressed-file-suffix :foreground (doom-blend orange bg 0.6))
    (diredfl-date-time              :foreground cyan :weight 'light)
    (diredfl-deletion               :foreground red :background (doom-blend red bg 0.2) :weight 'bold)
    (diredfl-deletion-file-name     :foreground red :background (doom-blend red bg 0.2))
    (diredfl-dir-heading            :foreground blue :weight 'bold)
    (diredfl-dir-name               :foreground blue)
    (diredfl-dir-priv               :foreground blue)
    (diredfl-exec-priv              :foreground green)
    (diredfl-executable-tag         :foreground green)
    (diredfl-file-name              :foreground fg)
    (diredfl-file-suffix            :foreground (doom-blend fg bg 0.6))
    (diredfl-flag-mark              :foreground yellow :background (doom-blend yellow bg 0.2) :weight 'bold)
    (diredfl-flag-mark-line         :background (doom-blend yellow bg 0.1))
    (diredfl-ignored-file-name      :foreground comments)
    (diredfl-link-priv              :foreground violet)
    (diredfl-no-priv                :inherit 'shadow)
    (diredfl-number                 :foreground orange)
    (diredfl-other-priv             :foreground magenta)
    (diredfl-rare-priv              :foreground fg)
    (diredfl-read-priv              :foreground yellow)
    (diredfl-symlink                :foreground violet)
    (diredfl-tagged-autofile-name   :foreground base5)
    (diredfl-write-priv             :foreground red)
    ;;;; disk-usage
    (disk-usage-children            :foreground yellow)
    (disk-usage-percent             :foreground violet)
    (disk-usage-size                :foreground blue)
    (disk-usage-symlink             :foreground cyan :weight 'bold)
    ;;;; doom-themes
    (doom-themes-visual-bell :background error)
    ;;;; ediff <built-in>
    (ediff-fine-diff-A    :background (doom-blend selection bg 0.7) :weight 'bold :extend t)
    (ediff-fine-diff-B    :inherit 'ediff-fine-diff-A)
    (ediff-fine-diff-C    :inherit 'ediff-fine-diff-A)
    (ediff-current-diff-A :background (doom-blend selection bg 0.3) :extend t)
    (ediff-current-diff-B :inherit 'ediff-current-diff-A)
    (ediff-current-diff-C :inherit 'ediff-current-diff-A)
    (ediff-even-diff-A    :inherit 'hl-line)
    (ediff-even-diff-B    :inherit 'ediff-even-diff-A)
    (ediff-even-diff-C    :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-A     :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-B     :inherit 'ediff-odd-diff-A)
    (ediff-odd-diff-C     :inherit 'ediff-odd-diff-A)
    ;;;; elixir-mode <modes:elixir-mode>
    (elixir-atom-face (&light :foreground dark-blue)
                      (&dark  :foreground cyan))
    (elixir-attribute-face :foreground violet)
    ;;;; elscreen
    (elscreen-tab-background-face     :background bg)
    (elscreen-tab-control-face        :background bg     :foreground bg)
    (elscreen-tab-current-screen-face :background bg-alt :foreground fg)
    (elscreen-tab-other-screen-face   :background bg     :foreground fg-alt)
    ;;;; embark
    ((embark-target &inherit vertico-current))
    ;;;; enh-ruby-mode <modes:enh-ruby-mode>
    (enh-ruby-heredoc-delimiter-face :inherit 'font-lock-string-face)
    (enh-ruby-op-face                :foreground operators)
    (enh-ruby-regexp-delimiter-face  :inherit 'enh-ruby-regexp-face)
    (enh-ruby-regexp-face            :foreground constants)
    (enh-ruby-string-delimiter-face  :inherit 'font-lock-string-face)
    (erm-syn-errline                 :underline `(:style wave :color ,error))
    (erm-syn-warnline                :underline `(:style wave :color ,warning))
    ;;;; eshell <built-in>
    (eshell-prompt        :foreground highlight :weight 'bold)
    (eshell-ls-archive    :foreground magenta)
    (eshell-ls-backup     :foreground yellow)
    (eshell-ls-clutter    :foreground red)
    (eshell-ls-directory  :foreground blue)
    (eshell-ls-executable :foreground green)
    (eshell-ls-missing    :foreground red)
    (eshell-ls-product    :foreground orange)
    (eshell-ls-readonly   :foreground orange)
    (eshell-ls-special    :foreground violet)
    (eshell-ls-symlink    :foreground cyan)
    (eshell-ls-unreadable :foreground base5)
    ;;;; flycheck
    (flycheck-error          :underline `(:style wave :color ,red))
    (flycheck-warning        :underline `(:style wave :color ,yellow))
    (flycheck-info           :underline `(:style wave :color ,green))
    (flycheck-fringe-error   :inherit 'fringe :foreground error)
    (flycheck-fringe-warning :inherit 'fringe :foreground warning)
    (flycheck-fringe-info    :inherit 'fringe :foreground success)
    ;;;; flycheck-posframe
    (flycheck-posframe-face            :inherit 'default)
    (flycheck-posframe-background-face :background bg-alt)
    (flycheck-posframe-error-face      :inherit 'flycheck-posframe-face :foreground error)
    (flycheck-posframe-info-face       :inherit 'flycheck-posframe-face :foreground fg)
    (flycheck-posframe-warning-face    :inherit 'flycheck-posframe-face :foreground warning)
    ;;;; flymake
    (flymake-error   :underline `(:style wave :color ,red))
    (flymake-note    :underline `(:style wave :color ,green))
    (flymake-warning :underline `(:style wave :color ,orange))
    ;;;; flyspell <built-in>
    (flyspell-incorrect :underline `(:style wave :color ,error) :inherit 'unspecified)
    (flyspell-duplicate :underline `(:style wave :color ,warning) :inherit 'unspecified)
    ;;;; flx-ido
    (flx-highlight-face :weight 'bold :foreground yellow :underline nil)
    ;;;; forge
    (forge-dimmed :inherit 'magit-dimmed)
    (forge-pullreq-open :inherit 'forge-issue-open)
    (forge-pullreq-merged :inherit 'forge-issue-completed)
    (forge-pullreq-rejected :inherit 'forge-issue-unplanned)
    ;;;; git-commit
    (git-commit-summary               :foreground strings)
    (git-commit-overlong-summary      :inherit 'error          :background base0 :slant 'italic :weight 'bold)
    (git-commit-nonempty-second-line  :inherit 'git-commit-overlong-summary)
    (git-commit-keyword               :foreground cyan         :slant 'italic)
    (git-commit-pseudo-header         :foreground doc-comments :slant 'italic)
    (git-commit-known-pseudo-header   :foreground doc-comments :weight 'bold     :slant 'italic)
    (git-commit-comment-branch-local  :foreground magenta)
    (git-commit-comment-branch-remote :foreground green)
    (git-commit-comment-detached      :foreground orange)
    (git-commit-comment-heading       :foreground keywords)
    (git-commit-comment-file          :foreground violet)
    (git-commit-comment-action)
    ;;;; git-gutter
    (git-gutter:modified :inherit 'fringe :foreground vc-modified)
    (git-gutter:added    :inherit 'fringe :foreground vc-added)
    (git-gutter:deleted  :inherit 'fringe :foreground vc-deleted)
    ;;;; git-gutter+
    (git-gutter+-modified :inherit 'fringe :foreground vc-modified :background 'unspecified)
    (git-gutter+-added    :inherit 'fringe :foreground vc-added    :background 'unspecified)
    (git-gutter+-deleted  :inherit 'fringe :foreground vc-deleted  :background 'unspecified)
    ;;;; git-gutter-fringe
    ((git-gutter-fr:modified &inherit git-gutter:modified))
    ((git-gutter-fr:added    &inherit git-gutter:added))
    ((git-gutter-fr:deleted  &inherit git-gutter:deleted))
    ;;;; helpful
    (helpful-heading :weight 'bold :height 1.2)
    ;;;; hi-lock <built-in>
    (hi-yellow   :background yellow)
    (hi-pink     :background magenta)
    (hi-red-b    :foreground red :weight 'bold)
    (hi-green    :background green)
    (hi-green-b  :foreground green :weight 'bold)
    (hi-blue     :background blue)
    (hi-blue-b   :foreground blue :weight 'bold)
    ;; (hi-black-b  :weight 'bold)
    ;; (hi-black-hb :inherit 'variable-pitch :weight 'bold :height 1.67)
    ;;;; hideshow <built-in>
    (+fold-hideshow-folded-face  ; this is defined in Doom Emacs, only
     :inherit 'font-lock-comment-face
     :weight 'light
     :background (doom-darken bg 0.15))
    ;;;; highlight-numbers-mode
    (highlight-numbers-number :inherit 'bold :foreground numbers)
    ;;;; highlight-indentation-mode
    (highlight-indentation-face                :inherit 'hl-line)
    (highlight-indentation-current-column-face :background base1)
    (highlight-indentation-guides-odd-face     :inherit 'highlight-indentation-face)
    (highlight-indentation-guides-even-face    :inherit 'highlight-indentation-face)
    ;;;; highlight-quoted-mode
    (highlight-quoted-symbol :foreground type)
    (highlight-quoted-quote  :foreground operators)
    ;;;; highlight-symbol
    (highlight-symbol-face
     (&dark  :background (doom-lighten region 0.1) :distant-foreground fg-alt)
     (&light :background (doom-darken region 0.1) :distant-foreground fg-alt))
    ;;;; highlight-thing
    (highlight-thing
     (&dark  :background (doom-lighten region 0.1) :distant-foreground fg-alt)
     (&light :background (doom-darken region 0.1) :distant-foreground fg-alt))
    ;;;; hl-fill-column-face
    (hl-fill-column-face :inherit '(hl-line shadow))
    ;;;; hl-line (built-in)
    (hl-line :background bg-alt :extend t)
    ;;;; hl-todo
    (hl-todo :foreground red :weight 'bold)
    ;;;; hlinum
    (linum-highlight-face :foreground fg :weight 'normal)
    ;;;; hydra
    (hydra-face-red      :foreground red     :weight 'bold)
    (hydra-face-blue     :foreground blue    :weight 'bold)
    (hydra-face-amaranth :foreground magenta :weight 'bold)
    (hydra-face-pink     :foreground violet  :weight 'bold)
    (hydra-face-teal     :foreground teal    :weight 'bold)
    ;;;; iedit
    (iedit-occurrence :foreground magenta :weight 'bold :inverse-video t)
    (iedit-read-only-occurrence :inherit 'region)
    ;;;; imenu-list
    ;; (imenu-list-entry-face)
    (imenu-list-entry-face-0 :foreground highlight)
    (imenu-list-entry-subalist-face-0 :inherit 'imenu-list-entry-face-0 :weight 'bold)
    (imenu-list-entry-face-1 :foreground green)
    (imenu-list-entry-subalist-face-1 :inherit 'imenu-list-entry-face-1 :weight 'bold)
    (imenu-list-entry-face-2 :foreground yellow)
    (imenu-list-entry-subalist-face-2 :inherit 'imenu-list-entry-face-2 :weight 'bold)
    ;;;; indent-guide
    ((indent-guide-face &inherit highlight-indentation-face))
    ;;;; isearch <built-in>
    (isearch :inherit 'lazy-highlight :weight 'bold)
    (isearch-fail :background error :foreground base0 :weight 'bold)
    ;;;; selectrum
    (selectrum-current-candidate :background region :extend t)
    ;;;; vertico
    (vertico-current :background region :extend t)
    ;;;; vertico-posframe
    ;;(vertico-posframe :inherit 'default)
    (vertico-posframe-border :background grey)
    (vertico-posframe-border-2 :background red)
    (vertico-posframe-border-3 :background green)
    (vertico-posframe-border-4 :background blue)
    (vertico-posframe-border-fallback :background yellow)
    ;;;; js2-mode <modes:js2-mode,js2-jsx-mode>
    (js2-function-param    :foreground variables)
    (js2-function-call     :foreground functions)
    (js2-object-property   :foreground violet)
    (js2-jsdoc-tag         :foreground doc-comments)
    (js2-external-variable :foreground operators)
    ;;;; keycast
    (keycast-command :inherit 'mode-line-emphasis)
    (keycast-key     :inherit '(bold mode-line-highlight))
    ;;;; ledger-mode <modes:ledger-mode>
    (ledger-font-posting-date-face    :foreground blue)
    (ledger-font-posting-amount-face  :foreground yellow)
    (ledger-font-posting-account-face :foreground base8)
    (ledger-font-payee-cleared-face   :foreground violet :weight 'bold)
    (ledger-font-payee-uncleared-face :foreground base5  :weight 'bold)
    (ledger-font-xact-highlight-face  :background base0)
    ;;;; linum <built-in>
    ((linum &inherit line-number))
    ;;;; linum-relative
    ((linum-relative-current-face &inherit line-number-current-line))
    ;;;; lui
    (lui-time-stamp-face :foreground violet)
    (lui-highlight-face :foreground highlight)
    (lui-button-face :foreground builtin :underline t)
    ;;;; magit
    (magit-bisect-bad        :foreground red)
    (magit-bisect-good       :foreground green)
    (magit-bisect-skip       :foreground orange)
    (magit-blame-hash        :foreground cyan)
    (magit-blame-date        :foreground red)
    (magit-blame-heading     :foreground orange :background base3 :extend t)
    (magit-branch-current    :foreground blue)
    (magit-branch-local      :foreground cyan)
    (magit-branch-remote     :foreground green)
    (magit-cherry-equivalent :foreground violet)
    (magit-cherry-unmatched  :foreground cyan)
    (magit-diff-added             :foreground (doom-darken vc-added 0.2)  :background (doom-blend vc-added bg 0.1) :extend t)
    (magit-diff-added-highlight   :foreground vc-added                    :background (doom-blend vc-added bg 0.2) :weight 'bold :extend t)
    (magit-diff-base              :foreground (doom-darken orange 0.2) :background (doom-blend orange bg 0.1) :extend t)
    (magit-diff-base-highlight    :foreground orange                   :background (doom-blend orange bg 0.2) :weight 'bold :extend t)
    (magit-diff-context           :foreground (doom-darken fg 0.4) :background bg :extend t)
    (magit-diff-context-highlight :foreground fg                   :background bg-alt :extend t)
    (magit-diff-file-heading           :foreground fg :weight 'bold :extend t)
    (magit-diff-file-heading-selection :foreground magenta               :background dark-blue :weight 'bold :extend t)
    (magit-diff-hunk-heading           :foreground bg                    :background (doom-blend violet bg 0.3) :extend t)
    (magit-diff-hunk-heading-highlight :foreground bg                    :background violet :weight 'bold :extend t)
    (magit-diff-lines-heading          :foreground yellow :background red :extend t :extend t)
    (magit-diff-removed                :foreground (doom-darken vc-deleted 0.2) :background (doom-blend vc-deleted base3 0.1) :extend t)
    (magit-diff-removed-highlight      :foreground vc-deleted                   :background (doom-blend vc-deleted base3 0.2) :weight 'bold :extend t)
    (magit-diffstat-added              :foreground vc-added)
    (magit-diffstat-removed            :foreground vc-deleted)
    (magit-dimmed :foreground fg-alt)
    (magit-hash :foreground comments)
    (magit-header-line :background dark-blue :foreground base8 :weight 'bold
                       :box `(:line-width 3 :color ,dark-blue))
    (magit-filename :foreground violet)
    (magit-log-author :foreground orange)
    (magit-log-date :foreground blue)
    (magit-log-graph :foreground comments)
    (magit-process-ng :inherit 'error)
    (magit-process-ok :inherit 'success)
    (magit-reflog-amend :foreground magenta)
    (magit-reflog-checkout :foreground blue)
    (magit-reflog-cherry-pick :foreground green)
    (magit-reflog-commit :foreground green)
    (magit-reflog-merge :foreground green)
    (magit-reflog-other :foreground cyan)
    (magit-reflog-rebase :foreground magenta)
    (magit-reflog-remote :foreground cyan)
    (magit-reflog-reset :inherit 'error)
    (magit-refname :foreground comments)
    (magit-section-heading :foreground blue :weight 'bold :extend t)
    (magit-section-heading-selection :foreground orange :weight 'bold :extend t)
    (magit-section-highlight :inherit 'hl-line)
    (magit-section-secondary-heading :foreground violet :weight 'bold :extend t)
    (magit-sequence-drop :foreground red)
    (magit-sequence-head :foreground blue)
    (magit-sequence-part :foreground orange)
    (magit-sequence-stop :foreground green)
    (magit-signature-bad :inherit 'error)
    (magit-signature-error :inherit 'error)
    (magit-signature-expired :foreground orange)
    (magit-signature-good :inherit 'success)
    (magit-signature-revoked :foreground magenta)
    (magit-signature-untrusted :foreground yellow)
    (magit-tag :foreground yellow)
    ;;;; make-mode <built-in> <modes:makefile-mode,makefile-automake-mode,makefile-makepp-mode,makefile-gmake-mode,makefile-imake-mode,makefile-bsdmake-mode>
    (makefile-targets :foreground blue)
    ;;;; man <built-in> <mode:Man-mode>
    (Man-overstrike :inherit 'bold :foreground operators)
    (Man-underline :inherit 'underline :foreground keywords)
    ;;;; markdown-mode <modes:markdown-mode,gfm-mode>
    (markdown-header-face           :inherit 'bold :foreground highlight)
    (markdown-header-delimiter-face :inherit 'markdown-header-face)
    (markdown-metadata-key-face     :foreground red)
    (markdown-list-face             :foreground red)
    (markdown-link-face             :foreground highlight)
    (markdown-url-face              :foreground magenta :weight 'normal)
    (markdown-italic-face           :inherit 'italic :foreground violet)
    (markdown-bold-face             :inherit 'bold   :foreground orange)
    (markdown-markup-face           :foreground operators)
    (markdown-blockquote-face       :inherit 'italic :foreground doc-comments)
    (markdown-pre-face              :foreground strings)
    (markdown-code-face             :background base3 :extend t)
    (markdown-reference-face        :foreground doc-comments)
    (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face) :extend nil)
    (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
    (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
    (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
    (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
    (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)
    ;;;; marginalia
    (marginalia-documentation   :inherit 'font-lock-doc-face)
    (marginalia-file-priv-dir   :foreground blue)
    (marginalia-file-priv-exec  :foreground green)
    (marginalia-file-priv-link  :foreground violet)
    (marginalia-file-priv-other :foreground magenta)
    (marginalia-file-priv-rare  :foreground fg)
    (marginalia-file-priv-read  :foreground yellow)
    (marginalia-file-priv-write :foreground red)
    (marginalia-number          :foreground numbers)
    (marginalia-size            :foreground violet)
    (marginalia-lighter         :foreground violet)
    ;;;; message <built-in>
    (message-header-name       :foreground green)
    (message-header-subject    :foreground highlight :weight 'bold)
    (message-header-to         :foreground highlight :weight 'bold)
    (message-header-cc         :inherit 'message-header-to :foreground (doom-darken highlight 0.15))
    (message-header-other      :foreground violet)
    (message-header-newsgroups :foreground yellow)
    (message-header-xheader    :foreground doc-comments)
    (message-separator         :foreground comments)
    (message-mml               :foreground comments :slant 'italic)
    ((message-cited-text   &inherit gnus-cite-1))
    ((message-cited-text-1 &inherit gnus-cite-2))
    ((message-cited-text-2 &inherit gnus-cite-3))
    ((message-cited-text-3 &inherit gnus-cite-4))
    ((message-cited-text-4 &inherit gnus-cite-5))
    ;;;; mic-paren
    (paren-face-match    :foreground red   :background base0 :weight 'ultra-bold)
    (paren-face-mismatch :foreground base0 :background red   :weight 'ultra-bold)
    (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)
    ;;;; minimap
    (minimap-current-line-face :background selection)
    (minimap-active-region-background :background vertical-bar)
    ;; mm
    (mm-uu-extract :background (doom-blend highlight base2 0.07) :foreground (doom-blend highlight fg 0.15))
    ;;;; mmm-mode
    (mmm-init-submode-face :background (doom-blend red bg 0.1))
    (mmm-cleanup-submode-face :background (doom-blend yellow bg 0.1))
    (mmm-declaration-submode-face :background (doom-blend cyan bg 0.1))
    (mmm-comment-submode-face :background (doom-blend blue bg 0.1))
    (mmm-output-submode-face :background (doom-blend violet bg 0.1))
    (mmm-special-submode-face :background (doom-blend green bg 0.1))
    (mmm-code-submode-face :background bg-alt)
    (mmm-default-submode-face) ; make transparent
    ;;;; nav-flash
    (nav-flash-face :background selection :foreground base8 :weight 'bold)
    ;;;; nerd-icons
    (nerd-icons-blue       :foreground blue)
    (nerd-icons-blue-alt   :foreground teal)
    (nerd-icons-cyan       :foreground cyan)
    (nerd-icons-cyan-alt   :foreground cyan)
    (nerd-icons-dblue      :foreground dark-blue)
    (nerd-icons-dcyan      :foreground dark-cyan)
    (nerd-icons-dgreen     :foreground (doom-darken green 0.3))
    (nerd-icons-dmaroon    :foreground (doom-darken magenta 0.3))
    (nerd-icons-dorange    :foreground (doom-darken orange 0.3))
    (nerd-icons-dpink      :foreground (doom-lighten red 0.15))
    (nerd-icons-dpurple    :foreground (doom-darken violet 0.3))
    (nerd-icons-dred       :foreground (doom-darken red 0.3))
    (nerd-icons-dsilver    :foreground (doom-lighten grey 0.1))
    (nerd-icons-dyellow    :foreground (doom-darken yellow 0.3))
    (nerd-icons-green      :foreground green)
    (nerd-icons-lblue      :foreground (doom-lighten blue 0.3))
    (nerd-icons-lcyan      :foreground (doom-lighten cyan 0.3))
    (nerd-icons-lgreen     :foreground (doom-lighten green 0.3))
    (nerd-icons-lmaroon    :foreground (doom-lighten magenta 0.3))
    (nerd-icons-lorange    :foreground (doom-lighten orange 0.3))
    (nerd-icons-lpink      :foreground (doom-lighten red 0.55))
    (nerd-icons-lpurple    :foreground (doom-lighten violet 0.3))
    (nerd-icons-lred       :foreground (doom-lighten red 0.3))
    (nerd-icons-lsilver    :foreground (doom-lighten grey 0.7))
    (nerd-icons-lyellow    :foreground (doom-lighten yellow 0.3))
    (nerd-icons-maroon     :foreground magenta)
    (nerd-icons-orange     :foreground orange)
    (nerd-icons-pink       :foreground (doom-lighten red 0.35))
    (nerd-icons-purple     :foreground violet)
    (nerd-icons-purple-alt :foreground (doom-blend violet grey 0.15))
    (nerd-icons-red        :foreground red)
    (nerd-icons-red-alt    :foreground (doom-blend red grey 0.15))
    (nerd-icons-silver     :foreground (doom-lighten grey 0.45))
    (nerd-icons-yellow     :foreground yellow)
    ;;;; nerd-icons-completion
    (nerd-icons-completion-dir-face :foreground doc-comments)
    ;;;; nerd-icons-dired
    (nerd-icons-dired-dir-face    :foreground doc-comments)
    ;;;; nlinum
    ((nlinum-current-line &inherit line-number-current-line))
    ;;;; nlinum-hl
    ((nlinum-hl-face &inherit line-number-current-line))
    ;;;; nlinum-relative
    ((nlinum-relative-current-face &inherit line-number-current-line))
    ;;;; lsp-mode
    (lsp-face-highlight-textual
     (&all   :weight 'bold)
     (&light :background base3 :foreground base0 :distant-foreground base8)
     (&dark  :background (doom-blend highlight bg 0.3) :foreground base8 :distant-foreground base0))
    (lsp-face-highlight-read    :inherit 'lsp-face-highlight-textual)
    (lsp-face-highlight-write   :inherit 'lsp-face-highlight-textual)
    (lsp-headerline-breadcrumb-separator-face :inherit 'shadow)
    ;;;; lsp-ui
    (lsp-ui-doc-background :inherit 'tooltip)
    (lsp-ui-peek-filename :inherit 'mode-line-buffer-id)
    (lsp-ui-peek-header :foreground fg :background (doom-lighten bg 0.1) :bold bold)
    (lsp-ui-peek-selection :foreground bg :background blue :bold bold)
    (lsp-ui-peek-list :background (doom-darken bg 0.1))
    (lsp-ui-peek-peek :background (doom-darken bg 0.1))
    (lsp-ui-peek-highlight :inherit 'isearch :box t)
    (lsp-ui-peek-line-number :foreground success)
    (lsp-ui-sideline-code-action :foreground (doom-blend highlight bg 0.85))
    (lsp-ui-sideline-current-symbol :inherit 'highlight)
    (lsp-ui-sideline-symbol-info :foreground (doom-blend comments bg 0.85)
                                 :background bg-alt :extend t)
    ;;;; objed
    (objed-mode-line :inherit 'warning :weight 'bold)
    (objed-hl        :inherit 'region :background (doom-blend region bg 0.5))
    ;;;; orderless
    (orderless-match-face-0 :weight 'bold :foreground (doom-blend blue    fg 0.6) :background (doom-blend blue    bg 0.1))
    (orderless-match-face-1 :weight 'bold :foreground (doom-blend magenta fg 0.6) :background (doom-blend magenta bg 0.1))
    (orderless-match-face-2 :weight 'bold :foreground (doom-blend green   fg 0.6) :background (doom-blend green   bg 0.1))
    (orderless-match-face-3 :weight 'bold :foreground (doom-blend yellow  fg 0.6) :background (doom-blend yellow  bg 0.1))
    ;;;; org <built-in> <modes:org-mode>
    (org-archived                 :foreground doc-comments)
    (org-block                    :background base3    :extend t)
    (org-block-background         :background base3    :extend t)
    (org-block-begin-line         :inherit 'org-block  :foreground comments)
    (org-block-end-line           :inherit 'org-block-begin-line)
    (org-checkbox                 :inherit 'org-todo)
    (org-checkbox-statistics-done :inherit 'org-done)
    (org-checkbox-statistics-todo :inherit 'org-todo)
    (org-cite                     :foreground (doom-blend teal fg 0.9))
    (org-cite-key                 :foreground (doom-blend teal fg 0.6) :underline t)
    (org-code                     :inherit 'org-block :foreground orange)
    (org-date                     :foreground yellow)
    (org-default                  :inherit 'variable-pitch)
    (org-document-info            :foreground builtin)
    (org-document-title           :foreground builtin         :weight 'bold)
    (org-done                     :inherit 'org-headline-done :strike-through nil :weight 'bold)
    (org-drawer                   :foreground comments)
    (org-ellipsis                 :foreground comments :underline nil)
    (org-footnote                 :foreground orange)
    (org-formula                  :foreground cyan)
    (org-headline-done            :foreground base5)
    (org-hide                     :foreground bg)
    (org-latex-and-related        :foreground base8           :weight 'bold)
    (org-link                     :inherit 'link              :foreground highlight)
    (org-list-dt                  :foreground highlight)
    (org-meta-line                :foreground doc-comments)
    (org-priority                 :foreground red)
    (org-property-value           :foreground doc-comments)
    (org-quote                    :inherit 'org-block :slant 'italic)
    (org-special-keyword          :foreground doc-comments)
    (org-table                    :foreground violet)
    (org-tag                      :foreground doc-comments :weight 'normal)
    (org-todo                     :foreground green :bold 'inherit)
    (org-verbatim                 :foreground green)
    (org-warning                  :foreground warning)
    ;; Omitted because we rely on style they inherit from the outline-N faces
    ;;(org-level-1)
    ;;(org-level-2)
    ;;(org-level-3)
    ;;(org-level-4)
    ;;(org-level-5)
    ;;(org-level-6)
    ;;(org-level-7)
    ;;(org-level-8)
    ;;;; org-agenda <built-in>
    (org-agenda-done :inherit 'org-done)
    (org-agenda-dimmed-todo-face :foreground comments)
    (org-agenda-date          :foreground violet :weight 'ultra-bold)
    (org-agenda-date-today    :foreground (doom-lighten violet 0.4)   :weight 'ultra-bold)
    (org-agenda-date-weekend  :foreground (doom-darken violet 0.4)  :weight 'ultra-bold)
    (org-agenda-structure     :foreground fg :weight 'ultra-bold)
    (org-agenda-clocking      :background (doom-blend blue bg 0.2))
    (org-upcoming-deadline         :foreground (doom-blend fg bg 0.8))
    (org-upcoming-distant-deadline :foreground (doom-blend fg bg 0.5))
    (org-scheduled            :foreground fg)
    (org-scheduled-today      :foreground base7)
    (org-scheduled-previously :foreground base8)
    (org-time-grid            :foreground comments)
    (org-sexp-date            :foreground fg)
    ;;;; org-habit
    (org-habit-clear-face          :weight 'bold :background base4)
    (org-habit-clear-future-face   :weight 'bold :background base3)
    (org-habit-ready-face          :weight 'bold :background (doom-blend blue bg-alt 0.5))
    (org-habit-ready-future-face   :weight 'bold :background (doom-blend blue bg-alt 0.3))
    (org-habit-alert-face          :weight 'bold :background (doom-blend yellow bg-alt 0.5))
    (org-habit-alert-future-face   :weight 'bold :background (doom-blend yellow bg-alt 0.3))
    (org-habit-overdue-face        :weight 'bold :background (doom-blend red bg-alt 0.5))
    (org-habit-overdue-future-face :weight 'bold :background (doom-blend red bg-alt 0.3))
    ;;;; org-journal <modes:org-journal-mode>
    (org-journal-highlight :foreground highlight)
    (org-journal-calendar-entry-face :foreground magenta :slant 'italic)
    (org-journal-calendar-scheduled-face :foreground red :slant 'italic)
    ;;;; org-pomodoro
    (org-pomodoro-mode-line :foreground red)
    (org-pomodoro-mode-line-overtime :foreground warning :weight 'bold)
    ;;;; org-ref
    (org-ref-acronym-face    :foreground violet)
    (org-ref-cite-face       :foreground yellow :weight 'light :underline t)
    (org-ref-glossary-face   :foreground magenta)
    (org-ref-label-face      :foreground blue)
    (org-ref-ref-face        :inherit 'link :foreground teal)
    ;;;; outline <built-in>
    ;; NOTE org-mode's org-level-N faces inherit these outline-N faces.
    (outline-1 :foreground blue                        :weight 'bold :extend t)
    (outline-2 :foreground magenta                     :weight 'bold :extend t)
    (outline-3 :foreground violet                      :weight 'bold :extend t)
    (outline-4 :foreground (doom-lighten blue 0.25)    :weight 'bold :extend t)
    (outline-5 :foreground (doom-lighten magenta 0.25) :weight 'bold :extend t)
    (outline-6 :foreground (doom-lighten blue 0.5)     :weight 'bold :extend t)
    (outline-7 :foreground (doom-lighten magenta 0.5)  :weight 'bold :extend t)
    (outline-8 :foreground (doom-lighten blue 0.8)     :weight 'bold :extend t)
    ;;;; parenface
    (paren-face :foreground comments)
    ;;;; parinfer
    (parinfer-pretty-parens:dim-paren-face :foreground base5)
    (parinfer-smart-tab:indicator-face :foreground base5)
    ;;;; perspective
    (persp-selected-face :foreground blue :weight 'bold)
    ;;;; persp-mode
    (persp-face-lighter-default :foreground highlight :weight 'bold)
    (persp-face-lighter-buffer-not-in-persp :foreground doc-comments)
    (persp-face-lighter-nil-persp :foreground comments)
    ;;;; pkgbuild-mode <modes:pkgbuild-mode>
    (pkgbuild-error-face :underline `(:style wave :color ,red))
    ;;;; popup
    (popup-face           :inherit 'tooltip)
    (popup-tip-face       :inherit 'popup-face :foreground violet :background base0)
    (popup-selection-face :background selection)
    ;;;; power
    (powerline-active0   :inherit 'mode-line :background bg)
    (powerline-active1   :inherit 'mode-line :background (doom-lighten 'bg 0.025))
    (powerline-active2   :inherit 'mode-line :foreground base8 :background (doom-lighten 'bg 0.08))
    (powerline-inactive0 :inherit 'mode-line-inactive :background base2)
    (powerline-inactive1 :inherit 'mode-line-inactive :background (doom-lighten 'base2 0.02))
    (powerline-inactive2 :inherit 'mode-line-inactive :background (doom-lighten 'base2 0.04))
    ;;;; rainbow-delimiters
    (rainbow-delimiters-depth-1-face :foreground blue)
    (rainbow-delimiters-depth-2-face :foreground magenta)
    (rainbow-delimiters-depth-3-face :foreground green)
    (rainbow-delimiters-depth-4-face :foreground violet)
    (rainbow-delimiters-depth-5-face :foreground teal)
    (rainbow-delimiters-depth-6-face :foreground blue)
    (rainbow-delimiters-depth-7-face :foreground magenta)
    (rainbow-delimiters-depth-8-face :foreground green)
    (rainbow-delimiters-depth-9-face :foreground violet)
    (rainbow-delimiters-base-error-face :inherit 'rainbow-delimiters-base-face :foreground error)
    (rainbow-delimiters-base-face :inherit 'default)
    (rainbow-delimiters-unmatched-face  :foreground red :weight 'bold :inverse-video t)
    (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)
    ;;;; re-builder <built-in>
    (reb-match-0 :foreground orange  :inverse-video t)
    (reb-match-1 :foreground magenta :inverse-video t)
    (reb-match-2 :foreground green   :inverse-video t)
    (reb-match-3 :foreground yellow  :inverse-video t)
    ;;;; rjsx-mode <modes:rjsx-mode>
    (rjsx-tag :foreground type)
    (rjsx-attr :foreground functions)
    ;;;; rpm-spec-mode <modes:rpm-spec-mode>
    (rpm-spec-macro-face        :foreground yellow)
    (rpm-spec-var-face          :foreground violet)
    (rpm-spec-tag-face          :foreground blue)
    (rpm-spec-obsolete-tag-face :foreground red)
    (rpm-spec-package-face      :foreground orange)
    (rpm-spec-dir-face          :foreground green)
    (rpm-spec-doc-face          :foreground orange)
    (rpm-spec-ghost-face        :foreground comments)
    (rpm-spec-section-face      :foreground magenta)
    ;;;; rst <built-in> <modes:rst-mode>
    (rst-block :inherit 'font-lock-constant-face)
    (rst-level-1 :inherit 'rst-adornment :weight 'bold)
    (rst-level-2 :inherit 'rst-adornment :weight 'bold)
    (rst-level-3 :inherit 'rst-adornment :weight 'bold)
    (rst-level-4 :inherit 'rst-adornment :weight 'bold)
    (rst-level-5 :inherit 'rst-adornment :weight 'bold)
    (rst-level-6 :inherit 'rst-adornment :weight 'bold)
    ;;;; sh-script <built-in> <modes:sh-mode,shell-script-mode>
    (sh-heredoc :inherit 'font-lock-string-face :weight 'normal)
    (sh-quoted-exec :inherit 'font-lock-preprocessor-face)
    ;;;; show-paren <built-in>
    ((show-paren-match &inherit paren-face-match))
    ((show-paren-mismatch &inherit paren-face-mismatch))
    ;;;; smartparens
    (sp-pair-overlay-face :background region)
    ((sp-show-pair-match-face    &inherit show-paren-match))
    ((sp-show-pair-mismatch-face &inherit show-paren-mismatch))
    ;;;; smerge-tool
    (smerge-lower :background (doom-blend green bg 0.2))
    (smerge-upper :background (doom-blend red base3 0.2))
    (smerge-base  :background (doom-blend blue bg 0.2))
    (smerge-markers :background comments :foreground bg :distant-foreground fg :weight 'bold)
    (smerge-refined-added   :inherit 'diff-added :inverse-video t)
    (smerge-refined-removed :inherit 'diff-removed :inverse-video t)
    ;; Emacs <25 compatibility
    ((smerge-mine  &inherit smerge-upper))
    ((smerge-other &inherit smerge-lower))
    ;;;; solaire-mode
    (solaire-default-face  :inherit 'default :background bg-alt)
    (solaire-hl-line-face  :inherit 'hl-line :background bg :extend t)
    (solaire-org-hide      :inherit 'org-hide :foreground bg-alt)
    ;;;; spaceline
    (spaceline-highlight-face   :background highlight)
    (spaceline-modified         :background vc-modified)
    (spaceline-unmodified       :background constants)
    (spaceline-python-venv      :foreground magenta :distant-foreground violet)
    (spaceline-flycheck-error   :inherit 'error     :distant-background base0)
    (spaceline-flycheck-warning :inherit 'warning   :distant-background base0)
    (spaceline-flycheck-info    :inherit 'success   :distant-background base0)
    (spaceline-evil-normal      :background blue)
    (spaceline-evil-insert      :background green)
    (spaceline-evil-emacs       :background cyan)
    (spaceline-evil-replace     :background orange)
    (spaceline-evil-visual      :background grey)
    (spaceline-evil-motion      :background magenta)
    ;;;; spell-fu
    (spell-fu-incorrect-face
     `((((supports :underline (:style wave)))
        (:underline (:style wave :color ,(car error))))
       (t (:inherit error :underline t))))
    ;;;; stripe-buffer
    (stripe-highlight
     (&light :background base5)
     (&dark  :background base3))
    ;;;; symbol-overlay
    (symbol-overlay-default-face
     (&dark  :background (doom-lighten region 0.1) :distant-foreground fg-alt)
     (&light :background (doom-darken region 0.1)  :distant-foreground fg-alt))
    (symbol-overlay-face-1 :background (doom-blend blue bg 0.4)    :distant-foreground fg-alt)
    (symbol-overlay-face-2 :background (doom-blend violet bg 0.4)  :distant-foreground fg-alt)
    (symbol-overlay-face-3 :background (doom-blend yellow bg 0.3)  :distant-foreground fg-alt)
    (symbol-overlay-face-4 :background (doom-blend orange bg 0.3)  :distant-foreground fg-alt)
    (symbol-overlay-face-5 :background (doom-blend red bg 0.3)     :distant-foreground fg-alt)
    (symbol-overlay-face-6 :background (doom-blend magenta bg 0.3) :distant-foreground fg-alt)
    (symbol-overlay-face-7 :background (doom-blend green bg 0.4)   :distant-foreground fg-alt)
    (symbol-overlay-face-8 :background (doom-blend cyan bg 0.2)    :distant-foreground fg-alt)
    ;;;; swiper
    (swiper-line-face    :background blue    :foreground base0)
    (swiper-match-face-1 :inherit 'unspecified :background base0   :foreground base5)
    (swiper-match-face-2 :inherit 'unspecified :background orange  :foreground base0 :weight 'bold)
    (swiper-match-face-3 :inherit 'unspecified :background magenta :foreground base0 :weight 'bold)
    (swiper-match-face-4 :inherit 'unspecified :background green   :foreground base0 :weight 'bold)
    ;;;; tabbar
    (tabbar-default             :foreground bg :background bg :height 1.0)
    (tabbar-highlight           :foreground fg :background selection :distant-foreground bg)
    (tabbar-button              :foreground fg :background bg)
    (tabbar-button-highlight    :inherit 'tabbar-button :inverse-video t)
    (tabbar-modified            :inherit 'tabbar-default :foreground red :weight 'bold)
    (tabbar-unselected          :inherit 'tabbar-default :foreground base5)
    (tabbar-unselected-modified :inherit 'tabbar-modified)
    (tabbar-selected
     :inherit 'tabbar-default :weight 'bold
     :foreground fg :background bg-alt)
    (tabbar-selected-modified :inherit 'tabbar-selected :foreground green)
    ;;;; term <built-in>
    (term               :foreground fg)
    (term-bold          :weight 'bold)
    (term-color-black   :background base0   :foreground base0)
    (term-color-red     :background red     :foreground red)
    (term-color-green   :background green   :foreground green)
    (term-color-yellow  :background yellow  :foreground yellow)
    (term-color-blue    :background blue    :foreground blue)
    (term-color-magenta :background magenta :foreground magenta)
    (term-color-cyan    :background cyan    :foreground cyan)
    (term-color-white   :background base8   :foreground base8)
    ;;;; terraform-mode
    (terraform--resource-type-face :foreground type)
    (terraform--resource-name-face :foreground strings)
    ;;;; tldr
    (tldr-command-itself   :foreground bg :background green :weight 'semi-bold)
    (tldr-title            :foreground yellow :bold t :height 1.4)
    (tldr-description      :foreground fg :weight 'semi-bold)
    (tldr-introduction     :foreground (doom-blend blue bg 0.8) :weight 'semi-bold)
    (tldr-code-block       :foreground green :background region :weight 'semi-bold)
    (tldr-command-argument :foreground fg :background region)
    ;;;; typescript-mode <modes:typescript-mode,typescript-tsx-mode>
    (typescript-jsdoc-tag :foreground doc-comments)
    (typescript-jsdoc-type :foreground (doom-darken doc-comments 0.15))
    (typescript-jsdoc-value :foreground (doom-lighten doc-comments 0.15))
    ;;;; treemacs
    (treemacs-directory-face        :foreground fg)
    (treemacs-file-face             :foreground fg)
    (treemacs-fringe-indicator-face :foreground highlight)
    (treemacs-git-added-face        :foreground vc-added)
    (treemacs-git-conflict-face     :foreground red)
    (treemacs-git-modified-face     :foreground violet)
    (treemacs-git-untracked-face    :inherit 'font-lock-doc-face)
    (treemacs-on-failure-pulse-face :foreground base0 :background error   :extend t)
    (treemacs-on-success-pulse-face :foreground base0 :background success :extend t)
    (treemacs-root-face             :inherit 'font-lock-string-face :weight 'bold       :height 1.2)
    (treemacs-tags-face             :foreground highlight)
    ;;;; treemacs-nerd-icons
    (treemacs-nerd-icons-file-face :foreground doc-comments)
    (treemacs-nerd-icons-root-face :inherit 'font-lock-string-face :weight 'bold :height 1.2)
    ;;;; ts-fold
    (ts-fold-fringe-face)
    ((ts-fold-replacement-face &inherit +fold-hideshow-folded-face))
    ((ts-fold-replacement-mouse-face &inherit +fold-hideshow-folded-face)
     :box '(:line-width -1 :style released-button))
    ;;;; twittering-mode
    (twitter-divider  ; custom face in Doom Emacs
     (&light :underline `(:color ,(doom-lighten vertical-bar 0.2)))
     (&dark  :underline `(:color ,(doom-darken vertical-bar 0.2))))
    ;;;; undo-tree
    (undo-tree-visualizer-default-face       :foreground base5)
    (undo-tree-visualizer-current-face       :foreground green :weight 'bold)
    (undo-tree-visualizer-unmodified-face    :foreground base5)
    (undo-tree-visualizer-active-branch-face :foreground blue)
    (undo-tree-visualizer-register-face      :foreground yellow)
    ;;;; vimish-fold
    (vimish-fold-overlay :inherit 'font-lock-comment-face :background base0 :weight 'light)
    (vimish-fold-fringe  :foreground magenta)
    ;;;; visual-regexp
    (vr/group-0 :background blue    :foreground bg)
    (vr/group-1 :background magenta :foreground bg)
    (vr/group-2 :background green   :foreground bg)
    (vr/match-0 :background (doom-blend green bg 0.2) :foreground fg)
    (vr/match-1 :background (doom-blend green bg 0.4) :foreground fg)
    (vr/match-separator-face :inherit 'bold :foreground red)
    ;;;; volatile-highlights
    (vhl/default-face :background grey)
    ;;;; vterm
    (vterm-color-black   :background (doom-lighten base0 0.25)   :foreground base0)
    (vterm-color-red     :background (doom-lighten red 0.25)     :foreground red)
    (vterm-color-green   :background (doom-lighten green 0.25)   :foreground green)
    (vterm-color-yellow  :background (doom-lighten yellow 0.25)  :foreground yellow)
    (vterm-color-blue    :background (doom-lighten blue 0.25)    :foreground blue)
    (vterm-color-magenta :background (doom-lighten magenta 0.25) :foreground magenta)
    (vterm-color-cyan    :background (doom-lighten cyan 0.25)    :foreground cyan)
    (vterm-color-white   :background (doom-lighten base8 0.25)   :foreground base8)
    ;;;; web-mode <modes:web-mode>
    (web-mode-block-control-face     :foreground builtin)
    (web-mode-block-delimiter-face   :foreground builtin)
    (web-mode-css-property-name-face :foreground type)
    (web-mode-doctype-face           :foreground comments)
    (web-mode-html-tag-face          :foreground methods)
    (web-mode-html-tag-bracket-face  :foreground methods)
    (web-mode-html-attr-name-face    :foreground type)
    (web-mode-html-attr-value-face   :foreground strings)
    (web-mode-html-entity-face       :foreground cyan :inherit 'italic)
    (web-mode-block-control-face     :foreground orange)
    (web-mode-html-tag-bracket-face  :foreground operators)
    (web-mode-json-key-face          :foreground strings)
    (web-mode-json-context-face      :foreground strings)
    (web-mode-keyword-face           :foreground keywords)
    (web-mode-string-face            :foreground strings)
    (web-mode-type-face              :foreground type)
    ;;;; wgrep <built-in>
    (wgrep-face :weight 'bold :foreground green :background base5)
    (wgrep-delete-face :foreground base3 :background red)
    (wgrep-done-face   :foreground blue)
    (wgrep-file-face   :foreground comments)
    (wgrep-reject-face :foreground red :weight 'bold)
    ;;;; which-func
    (which-func :foreground blue)
    ;;;; which-key
    (which-key-key-face                   :foreground green)
    (which-key-group-description-face     :foreground violet)
    (which-key-command-description-face   :foreground blue)
    (which-key-local-map-description-face :foreground magenta)
    ;;;; whitespace <built-in>
    (whitespace-empty    :background base3)
    (whitespace-space    :foreground base4)
    (whitespace-newline  :foreground base4)
    (whitespace-tab
     :foreground base4
     :background (if (default-value 'indent-tabs-mode) 'unspecified base3))
    (whitespace-indentation
     :foreground base4
     :background (if (default-value 'indent-tabs-mode) base3 'unspecified))
    (whitespace-trailing :inherit 'trailing-whitespace)
    (whitespace-line     :background base0 :foreground red :weight 'bold)
    ;;;; widget
    (widget-button-pressed :foreground red)
    (widget-documentation  :foreground green)
    (widget-single-line-field :background base3 :distant-foreground bg)
    (widget-field :background base3 :distant-foreground bg
                  :box `(:line-width -1 :color ,grey) :extend t)
    ;;;; window-divider
    (window-divider :inherit 'vertical-border)
    (window-divider-first-pixel :inherit 'window-divider)
    (window-divider-last-pixel  :inherit 'window-divider)
    ;;;; window-tool-bar
    (window-tool-bar-button :background bg :foreground fg)
    (window-tool-bar-button-hover :inherit 'highlight :distant-foreground bg)
    (window-tool-bar-button-disabled :background bg-alt :foreground fg-alt)
    ;;;; winum
    (winum-face :inherit 'bold :foreground highlight)
    ;;;; woman <built-in>
    (woman-bold :inherit 'Man-overstrike)
    (woman-italic :inherit 'Man-underline)
    ;;;; xah-elisp-mode
    (xah-elisp-at-symbol     :inherit 'font-lock-warning-face)
    (xah-elisp-cap-variable  :inherit 'font-lock-preprocessor-face)
    (xah-elisp-command-face  :inherit 'font-lock-type-face)
    (xah-elisp-dollar-symbol :inherit 'font-lock-variable-name-face)
    ;;;; workgroups2
    (wg-current-workgroup-face :foreground base0 :background highlight)
    (wg-other-workgroup-face   :foreground base5)
    (wg-divider-face           :foreground grey)
    (wg-brace-face             :foreground highlight)
    ;;;; yasnippet
    (yas-field-highlight-face :inherit 'match)
    ;;;; xref <built-in>
    ((xref-file-header &inherit compilation-info))
    ((xref-line-number &inherit compilation-line-number))
    ((xref-match &inherit match)))
    ;;;; --- END Package faces ------------------

  "TODO")

(defgroup doom-themes nil
  "Options for doom-themes."
  :group 'faces)

(defcustom doom-themes-enable-bold nil
  "If nil, bold will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)

(defcustom doom-themes-enable-italic nil
  "If nil, italics will be disabled across all faces."
  :group 'doom-themes
  :type 'boolean)

;;
;;; API

(defvar doom-themes--colors nil)
(defvar doom--min-colors '(257 256 16))
(defvar doom--quoted-p nil)
(defvar doom-themes--faces nil)

(defun doom-themes--colors-p (item)
  (declare (pure t) (side-effect-free t))
  (when item
    (cond ((listp item)
           (let ((car (car item)))
             (cond ((memq car '(quote doom-color)) nil)

                   ((memq car '(backquote \`))
                    (let ((doom--quoted-p t))
                      (doom-themes--colors-p (cdr item))))

                   ((eq car '\,)
                    (let (doom--quoted-p)
                      (doom-themes--colors-p (cdr item))))

                   ((or (doom-themes--colors-p car)
                        (doom-themes--colors-p (cdr-safe item)))))))

          ((and (symbolp item)
                (not (keywordp item))
                (not doom--quoted-p)
                (not (equal (substring (symbol-name item) 0 1) "-"))
                (assq item doom-themes--colors))))))

(defun doom-themes--apply-faces (new-faces &optional default-faces)
  (declare (pure t) (side-effect-free t))
  (let ((default-faces (or default-faces doom-themes-base-faces))
        (faces (make-hash-table :test #'eq :size (+ (length default-faces) (length new-faces))))
        (directives (make-hash-table :test #'eq)))
    (dolist (spec (append (mapcar #'copy-sequence default-faces) new-faces))
      (if (listp (car spec))
          (cl-destructuring-bind (face action &optional arg) (car spec)
            (unless (assq face new-faces)
              (puthash face (list action arg (cdr spec))
                       directives)))
        (puthash (car spec) (cdr spec) faces)))
    (cl-loop for face being the hash-keys of directives
             for (action target spec) = (gethash face directives)
             unless (memq action '(&inherit &extend &override))
             do (error "Invalid operation (%s) for '%s' face" action face)
             if (eq (car spec) 'quote)
             do (error "Can't extend literal face spec (for '%s')" face)
             ;; TODO Add &all/&light/&dark extension support
             else if (memq (car spec) '(&all &light &dark))
             do (error "Can't extend face with &all, &light or &dark specs (for '%s')" face)
             else do
             (puthash face
                      (let ((old-spec (gethash (or target face) faces))
                            (plist spec))
                        ;; remove duplicates
                        (while (keywordp (car plist))
                          (setq old-spec (plist-put old-spec (car plist) (cadr plist))
                                plist (cddr plist)))
                        old-spec)
                      faces))
    (let (results)
      (maphash (lambda (face plist)
                 (when (keywordp (car plist))
                   ;; TODO Clean up duplicates in &all/&light/&dark blocks
                   (dolist (prop (append (unless doom-themes-enable-bold   '(:weight normal :bold unspecified))
                                         (unless doom-themes-enable-italic '(:slant normal :italic unspecified))))
                     (when (and (plist-member plist prop)
                                (not (eq (plist-get plist prop) 'inherit)))
                       (plist-put plist prop
                                  (if (memq prop '(:weight :slant))
                                      (quote 'normal))))))
                 (push (cons face plist) results))
               faces)
      (nreverse results))))

(defun doom-themes--colorize (item type)
  (declare (pure t) (side-effect-free t))
  (when item
    (let ((doom--quoted-p doom--quoted-p))
      (cond ((listp item)
             (cond ((memq (car item) '(quote doom-color))
                    item)
                   ((eq (car item) 'doom-ref)
                    (doom-themes--colorize
                     (apply #'doom-ref (cdr item)) type))
                   ((let* ((item (append item nil))
                           (car (car item))
                           (doom--quoted-p
                            (cond ((memq car '(backquote \`)) t)
                                  ((eq car '\,) nil)
                                  (t doom--quoted-p))))
                      (cons car
                            (cl-loop
                             for i in (cdr item)
                             collect (doom-themes--colorize i type)))))))

            ((and (symbolp item)
                  (not (keywordp item))
                  (not doom--quoted-p)
                  (not (equal (substring (symbol-name item) 0 1) "-"))
                  (assq item doom-themes--colors))
             `(doom-color ',item ',type))

            (item)))))

(defun doom-themes--build-face (face)
  (declare (pure t) (side-effect-free t))
  `(list
    ',(car face)
    ,(let ((face-body (cdr face)))
       (cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                (if (doom-themes--colors-p real-attrs)
                    (dolist (cl doom--min-colors `(list ,@(nreverse defs)))
                      (push `(list '((class color) (min-colors ,cl))
                                   (list ,@(doom-themes--colorize real-attrs cl)))
                            defs))
                  `(list (list 't (list ,@real-attrs))))))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))

             ((let (all-attrs defs)
                (dolist (attrs face-body `(list ,@(nreverse defs)))
                  (cond ((eq (car attrs) '&all)
                         (setq all-attrs (append all-attrs (cdr attrs))))

                        ((memq (car attrs) '(&dark &light))
                         (let ((bg (if (eq (car attrs) '&dark) 'dark 'light))
                               (real-attrs (append all-attrs (cdr attrs) '())))
                           (cond ((doom-themes--colors-p real-attrs)
                                  (dolist (cl doom--min-colors)
                                    (push `(list '((class color) (min-colors ,cl) (background ,bg))
                                                 (list ,@(doom-themes--colorize real-attrs cl)))
                                          defs)))

                                 ((push `(list '((background ,bg)) (list ,@real-attrs))
                                        defs)))))))))))))


;;
;;; Color helper functions

;; Shamelessly *borrowed* from solarized
;;;###autoload
(defun doom-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

;;;###autoload
(defun doom-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (when (and color1 color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (doom-blend (doom-color color1) (doom-color color2) alpha))

          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (doom-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (doom-name-to-rgb color1)
                           for other in (doom-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (color1))))

;;;###autoload
(defun doom-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (doom-darken (doom-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-darken c alpha)))

        ((doom-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun doom-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((and color (symbolp color))
         (doom-lighten (doom-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (doom-lighten c alpha)))

        ((doom-blend color "#FFFFFF" (- 1 alpha)))))

;;;###autoload
(defun doom-color (name &optional type)
  "Retrieve a specific color named NAME (a symbol) from the current theme."
  (let ((colors (if (listp name)
                    name
                  (cdr-safe (assq name doom-themes--colors)))))
    (and colors
         (cond ((listp colors)
                (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                  (if (> i (1- (length colors)))
                      (car (last colors))
                    (nth i colors))))
               (t colors)))))

;;;###autoload
(defun doom-ref (face prop &optional class)
  "TODO"
  (let ((spec (or (cdr (assq face doom-themes--faces))
                  (error "Couldn't find the '%s' face" face))))
    (when (memq (car spec) '(quote backquote \`))
      (user-error "Can't fetch the literal spec for '%s'" face))
    (when class
      (setq spec (cdr (assq class spec)))
      (unless spec
        (error "Couldn't find the '%s' class in the '%s' face"
               class face)))
    (unless (plist-member spec prop)
      (error "Couldn't find the '%s' property in the '%s' face%s"
             prop face (if class (format "'s '%s' class" class) "")))
    (plist-get spec prop)))


;;
;;; Defining themes

(defun doom-themes-prepare-facelist (custom-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (declare (pure t) (side-effect-free t))
  (setq doom-themes--faces (doom-themes--apply-faces custom-faces))
  (mapcar #'doom-themes--build-face doom-themes--faces))

(defmacro def-doom-theme (name docstring defs &optional extra-faces extra-vars)
  "Define a DOOM theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (let ((doom-themes--colors defs))
    `(let* ((bold   doom-themes-enable-bold)
            (italic doom-themes-enable-italic)
            ,@defs)
       (setq doom-themes--colors
             (list ,@(cl-loop for (var val) in defs
                              collect `(cons ',var ,val))))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces
        ',name ,@(doom-themes-prepare-facelist extra-faces))
       (unless bold (set-face-bold 'bold 'unspecified))
       (unless italic (set-face-italic 'italic 'unspecified))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

;;
;;; Variables

(defgroup solarized-theme nil
  "Options for the `solarized' theme."
  :group 'doom-themes)

(defcustom solarized-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'solarized-theme
  :type 'boolean)

;;
;;; Theme definition

(def-doom-theme solarized
                "A light theme inspired by Solarized light"

                ;; name        default   256       16
                ((bg         '("#FDF6E3" "#FDF6E3" "white"        ))
                 (fg         '("#556b72" "#556b72" "black"        ))

                 ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                 ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                 ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                 ;; and `doom-blend' helper functions.
                 (bg-alt     '("#EEE8D5" "#EEE8D5" "white"        ))
                 (fg-alt     '("#7B8787" "#7B8787" "brightwhite"  ))

                 ;; These should represent a spectrum from bg to fg, where base0 is a starker
                 ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                 ;; dark grey, base0 should be white and base8 should be black.
                 (base0      '("#FFFBF0" "#FFFBF0" "white"        ))
                 (base1      '("#FCF8ED" "#FCF8ED" "brightblack"  ))
                 (base2      '("#FCF7E8" "#FCF7E8" "brightblack"  ))
                 (base3      '("#F2E6CE" "#F2E6CE" "brightblack"  ))
                 (base4      '("#E1DBCD" "#E1DBCD" "brightblack"  ))
                 (base5      '("#D6D6D6" "#D6D6D6" "brightblack"  ))
                 (base6      '("#96A7A9" "#96A7A9" "brightblack"  ))
                 (base7      '("#788484" "#788484" "brightblack"  ))
                 (base8      '("#626C6C" "#626C6C" "black"        ))

                 (grey       base4)
                 (red        '("#dc322f" "#dc322f" "red"          ))
                 (orange     '("#cb4b16" "#cb4b16" "brightred"    ))
                 (green      '("#859900" "#859900" "green"        ))
                 (teal       '("#35a69c" "#35a69c" "brightgreen"  ))
                 (yellow     '("#b58900" "#b58900" "yellow"       ))
                 (blue       '("#268bd2" "#268bd2" "brightblue"   ))
                 (dark-blue  '("#3F88AD" "#3F88AD" "blue"         ))
                 (magenta    '("#d33682" "#d33682" "magenta"      ))
                 (violet     '("#6c71c4" "#6c71c4" "brightmagenta"))
                 (cyan       '("#2aa198" "#2aa198" "brightcyan"   ))
                 (dark-cyan  '("#204052" "#204052" "cyan"         ))

                 ;; face categories -- required for all themes
                 (highlight      blue)
                 (vertical-bar   base4)
                 (selection      dark-blue)
                 (builtin        magenta)
                 (comments       (if solarized-brighter-comments
                                     (doom-lighten teal 0.25)
                                   base6))
                 (doc-comments   teal)
                 (constants      violet)
                 (functions      magenta)
                 (keywords       green)
                 (methods        cyan)
                 (operators      blue)
                 (type           yellow)
                 (strings        cyan)
                 (variables      blue)
                 (numbers        violet)
                 (region         `(,(doom-darken (car bg-alt) 0.1) ,@(doom-darken (cdr base0) 0.1)))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red))

  ;;;; Base theme face overrides
                (((font-lock-comment-face &override)
                  :slant 'italic
                  :background (if solarized-brighter-comments
                                  (doom-blend teal base0 0.07)
                                'unspecified))
                 ((font-lock-type-face &override) :slant 'italic)
                 ((font-lock-builtin-face &override) :slant 'italic)
                 ((font-lock-function-name-face &override) :foreground type)
                 ((font-lock-keyword-face &override) :weight 'bold)
                 ((font-lock-constant-face &override) :weight 'bold)
                 (hl-line :background base3)
                 ((line-number &override) :foreground base6)
                 ((line-number-current-line &override) :foreground fg :background region :weight 'bold)

   ;;;; elscreen
                 (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; css-mode <built-in> / scss-mode
                 (css-proprietary-property :foreground orange)
                 (css-property             :foreground green)
                 (css-selector             :foreground blue)
   ;;;; lsp-ui
                 (lsp-ui-sideline-code-action :foreground blue)
   ;;;; markdown-mode
                 (markdown-markup-face :foreground base5)
                 (markdown-header-face :inherit 'bold :foreground red)
                 ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; ivy
                 (ivy-current-match :background (doom-lighten yellow 0.65) :distant-foreground fg)
                 (ivy-minibuffer-match-face-1 :foreground blue :background base3 :weight 'bold)
                 (ivy-minibuffer-match-face-2 :foreground magenta :background base3 :weight 'bold)
                 (ivy-minibuffer-match-face-3 :foreground green   :background base3 :weight 'bold)
                 (ivy-minibuffer-match-face-4 :foreground yellow  :background base3 :weight 'bold)
                 (ivy-minibuffer-match-highlight :foreground violet :weight 'bold)
   ;;;; swiper
                 (swiper-match-face-1 :inherit 'ivy-minibuffer-match-face-1)
                 (swiper-match-face-2 :inherit 'ivy-minibuffer-match-face-2)
                 (swiper-match-face-3 :inherit 'ivy-minibuffer-match-face-3)
                 (swiper-match-face-4 :inherit 'ivy-minibuffer-match-face-4)
   ;;;; helm
                 (helm-selection :foreground base0 :weight 'bold :background blue)
   ;;;; company
                 (company-tooltip-selection :background blue :foreground base3)
   ;;;; org <built-in>
                 (org-block :background (doom-blend yellow bg 0.04) :extend t)
                 (org-block-background :background (doom-blend yellow bg 0.04))
                 (org-block-begin-line :background (doom-blend yellow bg 0.08) :extend t)
                 (org-block-end-line :background (doom-blend yellow bg 0.08) :extend t)
   ;;;; widget
                 (widget-field :foreground fg :background base3)
                 (widget-single-line-field :foreground fg :background base3)
   ;;;; latex
                 (font-latex-sedate-face :foreground base6)
   ;;;; notmuch
                 (notmuch-message-summary-face :foreground teal)
                 (notmuch-wash-cited-text :foreground base6))

  ;;;; Base theme variable overrides-
                ;; ()
                )

(custom-theme-set-variables
 'solarized
 `(rustic-ansi-faces
   (vconcat (mapcar #'doom-color '(bg red green yellow blue magenta cyan fg))))
 `(vc-annotate-color-map
   `(list (cons 20  ,(doom-color 'green))
          (cons 40  ,(doom-blend 'yellow 'green (/ 1.0 3)))
          (cons 60  ,(doom-blend 'yellow 'green (/ 2.0 3)))
          (cons 80  ,(doom-color 'yellow))
          (cons 100 ,(doom-blend 'orange 'yellow (/ 1.0 3)))
          (cons 120 ,(doom-blend 'orange 'yellow (/ 2.0 3)))
          (cons 140 ,(doom-color 'orange))
          (cons 160 ,(doom-blend 'magenta 'orange (/ 1.0 3)))
          (cons 180 ,(doom-blend 'magenta 'orange (/ 2.0 3)))
          (cons 200 ,(doom-color 'magenta))
          (cons 220 ,(doom-blend 'red 'magenta (/ 1.0 3)))
          (cons 240 ,(doom-blend 'red 'magenta (/ 2.0 3)))
          (cons 260 ,(doom-color 'red))
          (cons 280 ,(doom-blend 'grey 'red (/ 1.0 4)))
          (cons 300 ,(doom-blend 'grey 'red (/ 2.0 4)))
          (cons 320 ,(doom-blend 'grey 'red (/ 3.0 4)))
          (cons 340 ,(doom-color 'base5))
          (cons 360 ,(doom-color 'base5))))
 `(vc-annotate-very-old-color nil)
 `(vc-annotate-background (doom-color 'bg)))

;;; solarized-theme.el ends here
