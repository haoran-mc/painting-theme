;;; solarized-theme.el --- a light variant of Solarized -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: January 9, 2018 (#131)
;; Author: fuxialexander <https://github.com/fuxialexander>
;; Maintainer:
;; Source: https://github.com/bbatsov/solarized-emacs
;; Source: https://ethanschoonover.com/solarized
;;
(require 'cl-lib)

(deftheme solarized "A light theme inspired by Solarized light")

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
    (cond ((or (listp color1) (listp color2))
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
  (if (listp color)
      (cl-loop for c in color collect (doom-darken c alpha))
    (doom-blend color "#000000" (- 1 alpha))))

;;;###autoload
(defun doom-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (if (listp color)
      (cl-loop for c in color collect (doom-lighten c alpha))
    (doom-blend color "#FFFFFF" (- 1 alpha))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

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
    (lazy-highlight :background (doom-blend bg highlight 0.7) :foreground base0 :distant-foreground base8)
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
    (tab-bar :inherit 'tab-line)
    (tab-bar-tab :inherit 'tab-line-tab)
    (tab-bar-tab-inactive :inherit 'tab-line-tab-inactive)
    ;;;; Line numbers
    ;; 1. Line number faces must explicitly disable its text style attributes
    ;;    because nearby faces may "bleed" into the line numbers otherwise.
    ;; 2. All other line number plugin faces should &inherit from these.
    (line-number
     :inherit 'default
     :foreground base5 :distant-foreground 'unspecified
     :weight 'normal :italic 'unspecified
     :underline 'unspecified :strike-through 'unspecified)
    ;; ((line-number &override) :foreground base6)

    (line-number-current-line
     :inherit '(hl-line default)
     :foreground fg :distant-foreground 'unspecified
     :weight 'normal :italic 'unspecified
     :underline 'unspecified :strike-through 'unspecified)
    ;; ((line-number-current-line &override) :foreground fg :background region :weight 'bold)

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
    (font-latex-sedate-face       :foreground base6)
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
    ;;;; anzu
    (anzu-replace-highlight :background base0 :foreground red   :weight 'bold :strike-through t)
    (anzu-replace-to        :background base0 :foreground green :weight 'bold)
    ;;;; avy
    (avy-background-face :foreground comments)
    (avy-lead-face :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (avy-lead-face-0 :inherit 'avy-lead-face :background (doom-darken highlight 0.3))
    (avy-lead-face-1 :inherit 'avy-lead-face :background (doom-darken highlight 0.6))
    (avy-lead-face-2 :inherit 'avy-lead-face :background (doom-darken highlight 0.9))
    ;;;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-common                           :foreground highlight :distant-foreground base0 :weight 'bold)
    (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (company-tooltip-search-selection :background (doom-darken selection 0.25))
    (company-tooltip-selection  :background blue :foreground base3)
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
    ;;;; css-mode <built-in> / scss-mode
    (css-proprietary-property :foreground orange)
    (css-property             :foreground green)
    (css-selector             :foreground blue)
    ;;;; diff-hl
    (diff-hl-change :foreground vc-modified :background vc-modified)
    (diff-hl-delete :foreground vc-deleted :background vc-deleted)
    (diff-hl-insert :foreground vc-added :background vc-added)
    ;;;; diff-mode <built-in>
    (diff-added   :inherit 'hl-line :foreground green)
    (diff-changed :foreground violet)
    (diff-context :foreground (doom-lighten fg 0.12))
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
    (elixir-atom-face      :foreground dark-blue)
    (elixir-attribute-face :foreground violet)
    ;;;; elscreen
    (elscreen-tab-background-face     :background bg)
    (elscreen-tab-control-face        :background bg     :foreground bg)
    (elscreen-tab-current-screen-face :background bg-alt :foreground fg)
    (elscreen-tab-other-screen-face   :background "#353a42" :foreground "#1e2022")
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
    (highlight-symbol-face :background (doom-darken region 0.1) :distant-foreground fg-alt)
    ;;;; hl-fill-column-face
    (hl-fill-column-face :inherit '(hl-line shadow))
    ;;;; hl-line (built-in)
    (hl-line :background base3)
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
    (markdown-header-face :inherit 'bold :foreground red)
    (markdown-header-delimiter-face :inherit 'markdown-header-face)
    (markdown-metadata-key-face     :foreground red)
    (markdown-list-face             :foreground red)
    (markdown-link-face             :foreground highlight)
    (markdown-url-face              :foreground magenta :weight 'normal)
    (markdown-italic-face           :inherit 'italic :foreground violet)
    (markdown-bold-face             :inherit 'bold   :foreground orange)
    (markdown-markup-face           :foreground base5)
    (markdown-blockquote-face       :inherit 'italic :foreground doc-comments)
    (markdown-pre-face              :foreground strings)
    (markdown-code-face             :background (doom-lighten base3 0.05) :extend t)
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
    ;;;; mic-paren
    (paren-face-match    :foreground red   :background base0 :weight 'ultra-bold)
    (paren-face-mismatch :foreground base0 :background red   :weight 'ultra-bold)
    (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)
    ;;;; minimap
    (minimap-current-line-face :background selection)
    (minimap-active-region-background :background vertical-bar)
    ;;;; nav-flash
    (nav-flash-face :background selection :foreground base8 :weight 'bold)
    ;;;; lsp-mode
    (lsp-face-highlight-textual :background base3 :foreground base0 :distant-foreground base8)
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
    (lsp-ui-sideline-code-action :foreground blue)
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
    (org-block :background (doom-blend yellow bg 0.04) :extend t)
    (org-block-background :background (doom-blend yellow bg 0.04))
    (org-block-begin-line :background (doom-blend yellow bg 0.08) :extend t)
    (org-block-end-line :background (doom-blend yellow bg 0.08) :extend t)
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
    ;;;; pkgbuild-mode <modes:pkgbuild-mode>
    (pkgbuild-error-face :underline `(:style wave :color ,red))
    ;;;; popup
    (popup-face           :inherit 'tooltip)
    (popup-tip-face       :inherit 'popup-face :foreground violet :background base0)
    (popup-selection-face :background selection)
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
    (show-paren-match :inherit 'paren-face-match)
    (show-paren-mismatch :inherit 'paren-face-mismatch)
    ;;;; smartparens
    (sp-pair-overlay-face :background region)
    (sp-show-pair-match-face    :inherit 'show-paren-match)
    (sp-show-pair-mismatch-face :inherit 'show-paren-mismatch)
    ;;;; stripe-buffer
    (stripe-highlight :background base5)
    ;;;; symbol-overlay
    (symbol-overlay-default-face :background (doom-darken region 0.1)  :distant-foreground fg-alt)
    (symbol-overlay-face-1 :background (doom-blend blue bg 0.4)    :distant-foreground fg-alt)
    (symbol-overlay-face-2 :background (doom-blend violet bg 0.4)  :distant-foreground fg-alt)
    (symbol-overlay-face-3 :background (doom-blend yellow bg 0.3)  :distant-foreground fg-alt)
    (symbol-overlay-face-4 :background (doom-blend orange bg 0.3)  :distant-foreground fg-alt)
    (symbol-overlay-face-5 :background (doom-blend red bg 0.3)     :distant-foreground fg-alt)
    (symbol-overlay-face-6 :background (doom-blend magenta bg 0.3) :distant-foreground fg-alt)
    (symbol-overlay-face-7 :background (doom-blend green bg 0.4)   :distant-foreground fg-alt)
    (symbol-overlay-face-8 :background (doom-blend cyan bg 0.2)    :distant-foreground fg-alt)
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
    ;;;; vimish-fold
    (vimish-fold-overlay :inherit 'font-lock-comment-face :background base0 :weight 'light)
    (vimish-fold-fringe  :foreground magenta)
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
    (xref-file-header :inherit 'compilation-info)
    (xref-line-number :inherit 'compilation-line-number)
    (xref-match :inherit 'match))
    ;;;; --- END Package faces ------------------

  "TODO")

(defvar doom-themes--colors nil)

(defun doom-themes--build-face (face)
  (declare (pure t) (side-effect-free t))
  `(list
    ',(car face)
    ,(let ((face-body (cdr face)))
       (cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                `(list (list 't (list ,@real-attrs)))))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))))))

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

(defun doom-themes-prepare-facelist ()
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (declare (pure t) (side-effect-free t))
  (mapcar #'doom-themes--build-face doom-themes-base-faces))

(defmacro def-doom-theme (name docstring defs)
  "Define a DOOM theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (let ((doom-themes--colors defs))
    `(let* ((bold   nil)
            (italic nil)
            ,@defs)
       (setq doom-themes--colors
             (list ,@(cl-loop for (var val) in defs
                              collect `(cons ',var ,val))))
       (custom-theme-set-faces
        ',name ,@(doom-themes-prepare-facelist))
       (set-face-bold 'bold 'unspecified)
       (set-face-italic 'italic 'unspecified))))

(def-doom-theme solarized
                "A light theme inspired by Solarized light"

                ;; name        default   256       16
                ((bg         "#FDF6E3")
                 (fg         "#556b72")

                 ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
                 ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
                 ;; or region), especially when paired with the `doom-darken', `doom-lighten',
                 ;; and `doom-blend' helper functions.
                 (bg-alt     "#EEE8D5")
                 (fg-alt     "#7B8787")

                 ;; These should represent a spectrum from bg to fg, where base0 is a starker
                 ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
                 ;; dark grey, base0 should be white and base8 should be black.
                 (base0      "#FFFBF0")
                 (base1      "#FCF8ED")
                 (base2      "#FCF7E8")
                 (base3      "#F2E6CE")
                 (base4      "#E1DBCD")
                 (base5      "#D6D6D6")
                 (base6      "#96A7A9")
                 (base7      "#788484")
                 (base8      "#626C6C")

                 (grey       base4)
                 (red        "#dc322f")
                 (orange     "#cb4b16")
                 (green      "#859900")
                 (teal       "#35a69c")
                 (yellow     "#b58900")
                 (blue       "#268bd2")
                 (dark-blue  "#3F88AD")
                 (magenta    "#d33682")
                 (violet     "#6c71c4")
                 (cyan       "#2aa198")
                 (dark-cyan  "#204052")

                 ;; face categories -- required for all themes
                 (highlight      blue)
                 (vertical-bar   base4)
                 (selection      dark-blue)
                 (builtin        magenta)
                 (comments       base6)
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
                 (region         (doom-darken bg-alt 0.1))
                 (error          red)
                 (warning        yellow)
                 (success        green)
                 (vc-modified    orange)
                 (vc-added       green)
                 (vc-deleted     red)))

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

(provide-theme 'solarized)
;;; solarized-theme.el ends here
