(require 'cl-lib)

(deftheme lazycat "A dark theme inspired by Atom One Dark")

;;;###autoload
(defun lazycat-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

;;;###autoload
(defun lazycat-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (when (and color1 color2)
    (cond ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (lazycat-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (lazycat-name-to-rgb color1)
                           for other in (lazycat-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (color1))))

;;;###autoload
(defun lazycat-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((listp color)
         (cl-loop for c in color collect (lazycat-darken c alpha)))

        ((lazycat-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun lazycat-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((listp color)
         (cl-loop for c in color collect (lazycat-lighten c alpha)))

        ((lazycat-blend color "#FFFFFF" (- 1 alpha)))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(let* ((bg             "#242525")
       (bg-alt         "#333333")
       (base0          "#1B2229")
       (base1          "#1C1F24")
       (base2          "#202328")
       (base3          "#23272E")
       (base4          "#3F444A")
       (base5          "#5B6268")
       (base6          "#73797E")
       (base7          "#9CA0A4")
       (base8          "#DFDFDF")
       (fg             "#00CE00")
       (fg-alt         "green4")

       (grey           base4)
       (red            "#FF6C6B")
       (orange         "#DA8548")
       (green          "#98BE65")
       (teal           "#4DB5BD")
       (yellow         "#ECBE7B")
       (blue           "#51AFEF")
       (dark-blue      "#2257A0")
       (magenta        "#C678DD")
       (violet         "#A9A1E1")
       (cyan           "#46D9FF")
       (dark-cyan      "#5699AF")

       ;; face categories -- required for all themes
       (highlight      "green")
       (vertical-bar   (lazycat-darken base1 0.1))
       (selection      dark-blue)
       (builtin        "#00B8FF")
       (comments       "#A7A7A7")
       (doc-comments   "#AAAAAA")
       (constants      "#BD00FF")
       (functions      "gold2")
       (keywords       "#004FFF")
       (methods        cyan)
       (operators      "cyan3")
       (type           "#00B8FF")
       (strings        "#DFD67A")
       (variables      "gold2")
       (numbers        orange)
       (region         "#3F90F7")
       (region-fg      "#FFF")
       (error          red)
       (warning        yellow)
       (success        green)
       (vc-modified    orange)
       (vc-added       green)
       (vc-deleted     red)

       ;; custom categories
       (hidden         bg)
       (bold           nil)
       (italic         nil))

  (custom-theme-set-faces
   'lazycat

   ;; --- base faces -------------------------
   `(bold    ((t (:weight bold :foreground ,(unless bold base8)))))
   '(italic  ((t (:slant italic))))

   `(default ((t (:background ,bg :foreground ,fg))))
   `(fringe  ((t (:inherit default :foreground ,base4))))
   `(region              ((t (:background ,region :foreground ,region-fg))))
   `(highlight           ((t (:background ,highlight :foreground ,base0 :distant-foreground ,base8))))
   `(cursor              ((t (:background ,highlight))))
   `(shadow              ((t (:foreground ,base5))))
   `(minibuffer-prompt   ((t (:foreground ,highlight))))
   `(tooltip             ((t (:background ,base3 :foreground ,fg))))
   `(secondary-selection ((t (:background ,grey :extend t))))
   `(lazy-highlight      ((t (:background ,dark-blue :foreground ,base8 :distant-foreground ,base0 :weight bold))))
   `(match               ((t (:foreground ,green :background ,base0 :weight bold))))
   `(trailing-whitespace ((t (:background ,red))))
   `(nobreak-space       ((t (:inherit default :underline nil))))
   `(vertical-border     ((t (:background ,vertical-bar :foreground ,vertical-bar))))
   `(link                ((t (:foreground ,highlight :underline t))))

   `(error   ((t (:foreground ,error))))
   `(warning ((t (:foreground ,warning))))
   `(success ((t (:foreground ,success))))

   `(font-lock-builtin-face              ((t (:foreground ,builtin))))
   `(font-lock-comment-face              ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face    ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face                  ((t (:inherit font-lock-comment-face :foreground ,doc-comments))))
   `(font-lock-constant-face             ((t (:foreground ,constants))))
   `(font-lock-function-name-face        ((t (:foreground ,functions))))
   `(font-lock-keyword-face              ((t (:foreground ,keywords))))
   `(font-lock-string-face               ((t (:foreground ,strings))))
   `(font-lock-type-face                 ((t (:foreground ,type))))
   `(font-lock-variable-name-face        ((t (:foreground ,variables))))
   `(font-lock-warning-face              ((t (:inherit warning))))
   `(font-lock-negation-char-face        ((t (:inherit bold :foreground ,operators))))
   `(font-lock-preprocessor-face         ((t (:inherit bold :foreground ,operators))))
   `(font-lock-preprocessor-char-face    ((t (:inherit 'bold :foreground ,operators))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground ,operators))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground ,operators))))

   ;; mode-line
   `(mode-line           ((t (:background ,bg :foreground ,bg-alt))))
   `(mode-line-inactive  ((t (:background ,bg :foreground ,fg))))
   `(mode-line-emphasis  ((t (:foreground ,highlight :distant-foreground ,bg :height 0.1))))
   `(mode-line-highlight ((t (:inherit highlight :distant-foreground ,bg :height 0.1))))
   `(mode-line-buffer-id ((t (:weight bold :height 0.1))))

   ;; header-line
   `(header-line ((t (:inherit default :height ,(face-attribute 'default :height)))))

   ;; 1. Line number faces must explicitly disable its text style attributes
   ;;    because nearby faces may "bleed" into the line numbers otherwise.
   ;; 2. All other line number plugin faces should &inherit from these.
   `(line-number
     ((t (:inherit default
                   :foreground ,base5 :distant-foreground nil
                   :weight normal :italic nil :underline nil :strike-through nil))))
   `(line-number-current-line
     ((t (:inherit (hl-line default)
                   :foreground ,fg :distant-foreground nil
                   :weight normal :italic nil :underline nil :strike-through nil))))


   ;; --- built-in plugin faces --------------
   ;; cperl
   `(cperl-array-face          ((t (:weight bold :inherit font-lock-variable-name-face))))
   `(cperl-hash-face           ((t (:weight bold :slant italic :inherit font-lock-variable-name-face))))
   `(cperl-nonoverridable-face ((t (:inherit font-lock-builtin-face))))

   ;; compilation
   `(compilation-column-number  ((t (:inherit font-lock-comment-face))))
   `(compilation-line-number    ((t (:foreground ,highlight))))
   `(compilation-error          ((t (:inherit error :weight bold))))
   `(compilation-warning        ((t (:inherit warning :slant italic))))
   `(compilation-info           ((t (:inherit success))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error))))

   ;; custom
   `(custom-button                  ((t (:foreground ,blue   :background ,bg     :box (:line-width 1 :style none)))))
   `(custom-button-unraised         ((t (:foreground ,violet :background ,bg     :box (:line-width 1 :style none)))))
   `(custom-button-pressed-unraised ((t (:foreground ,bg     :background ,violet :box (:line-width 1 :style none)))))
   `(custom-button-pressed          ((t (:foreground ,bg     :background ,blue   :box (:line-width 1 :style none)))))
   `(custom-button-mouse            ((t (:foreground ,bg     :background ,blue   :box (:line-width 1 :style none)))))

   `(custom-variable-button   ((t (:foreground ,green :underline t))))
   `(custom-saved             ((t (:foreground ,green :background ,(lazycat-blend green bg 0.2) :bold bold))))
   `(custom-comment           ((t (:foreground ,fg :background ,region))))
   `(custom-comment-tag       ((t (:foreground ,grey))))
   `(custom-modified          ((t (:foreground ,blue :background ,(lazycat-blend blue bg 0.2)))))
   `(custom-variable-tag      ((t (:foreground ,magenta))))
   `(custom-visibility        ((t (:foreground ,blue :underline nil))))
   `(custom-group-subtitle    ((t (:foreground ,red))))
   `(custom-group-tag         ((t (:foreground ,violet))))
   `(custom-group-tag-1       ((t (:foreground ,blue))))
   `(custom-set               ((t (:foreground ,yellow :background ,bg))))
   `(custom-themed            ((t (:foreground ,yellow :background ,bg))))
   `(custom-invalid           ((t (:foreground ,red :background ,(lazycat-blend red bg 0.2)))))
   `(custom-variable-obsolete ((t (:foreground ,grey :background ,bg))))
   `(custom-state             ((t (:foreground ,green :background ,(lazycat-blend green bg 0.2)))))
   `(custom-changed           ((t (:foreground ,blue :background ,bg))))

   ;; dired
   `(dired-directory  ((t (:foreground ,builtin))))
   `(dired-ignored    ((t (:foreground ,comments))))
   `(dired-flagged    ((t (:foreground ,red))))
   `(dired-header     ((t (:foreground ,variables :weight bold))))
   `(dired-mark       ((t (:foreground ,orange :weight bold))))
   `(dired-marked     ((t (:foreground ,magenta :weight bold :inverse-video t))))
   `(dired-perm-write ((t (:foreground ,fg :underline t))))
   `(dired-symlink    ((t (:foreground ,cyan :weight bold))))
   `(dired-warning    ((t (:foreground ,warning))))

   ;; ediff
   `(ediff-fine-diff-A    ((t (:background ,(lazycat-blend selection bg 0.7) :weight bold :extend t))))
   `(ediff-fine-diff-B    ((t (:inherit ediff-fine-diff-A))))
   `(ediff-fine-diff-C    ((t (:inherit ediff-fine-diff-A))))
   `(ediff-current-diff-A ((t (:background ,(lazycat-blend selection bg 0.3) :extend t))))
   `(ediff-current-diff-B ((t (:inherit ediff-current-diff-A))))
   `(ediff-current-diff-C ((t (:inherit ediff-current-diff-A))))
   `(ediff-even-diff-A    ((t (:inherit hl-line))))
   `(ediff-even-diff-B    ((t (:inherit ediff-even-diff-A))))
   `(ediff-even-diff-C    ((t (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-A     ((t (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-B     ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-C     ((t (:inherit ediff-odd-diff-A))))

   ;; eshell
   `(eshell-prompt        ((t (:foreground ,highlight :weight bold))))
   `(eshell-ls-archive    ((t (:foreground ,magenta))))
   `(eshell-ls-backup     ((t (:foreground ,yellow))))
   `(eshell-ls-clutter    ((t (:foreground ,red))))
   `(eshell-ls-directory  ((t (:foreground ,blue))))
   `(eshell-ls-executable ((t (:foreground ,green))))
   `(eshell-ls-missing    ((t (:foreground ,red))))
   `(eshell-ls-product    ((t (:foreground ,orange))))
   `(eshell-ls-readonly   ((t (:foreground ,orange))))
   `(eshell-ls-special    ((t (:foreground ,violet))))
   `(eshell-ls-symlink    ((t (:foreground ,cyan))))
   `(eshell-ls-unreadable ((t (:foreground ,base5))))

   ;; fix-ido
   `(flx-highlight-face ((t (:weight bold :foreground ,yellow :underline nil))))

   ;; hi-lock
   `(hi-yellow  ((t (:background ,yellow))))
   `(hi-pink    ((t (:background ,magenta))))
   `(hi-red-b   ((t (:foreground ,red :weight bold))))
   `(hi-green   ((t (:background ,green))))
   `(hi-green-b ((t (:foreground ,green :weight bold))))
   `(hi-blue    ((t (:background ,blue))))
   `(hi-blue-b  ((t (:foreground ,blue :weight bold))))

   ;; hl-line
   `(hl-line ((t (:background ,bg-alt :extend t))))

   ;; ido
   `(ido-first-match ((t (:foreground ,orange))))
   `(ido-indicator   ((t (:foreground ,red :background ,bg))))
   `(ido-only-match  ((t (:foreground ,green))))
   `(ido-subdir      ((t (:foreground ,violet))))
   `(ido-virtual     ((t (:foreground ,comments))))

   ;; isearch
   `(isearch      ((t (:inherit lazy-highlight :weight bold))))
   `(isearch-fail ((t (:background ,error :foreground ,base0 :weight bold))))

   ;; linum totally inherit line-number
   `(linum ((t (:inherit default
                         :foreground ,base5 :distant-foreground nil
                         :weight normal :italic nil
                         :underline nil :strike-through nil))))

   ;; message
   `(message-header-name    ((t (:foreground ,green))))
   `(message-header-subject ((t (:foreground ,highlight :weight bold))))
   `(message-header-to      ((t (:foreground ,highlight :weight bold))))
   `(message-header-cc      ((t (:inherit message-header-to :foreground ,(lazycat-darken highlight 0.15)))))
   `(message-header-other   ((t (:foreground ,violet))))
   `(message-header-newsgroups ((t (:foreground ,yellow))))
   `(message-header-xheader ((t (:foreground ,doc-comments))))
   `(message-separator      ((t (:foreground ,comments))))
   `(message-mml            ((t (:foreground ,comments :slant italic))))
   `(message-cited-text     ((t (:foreground ,magenta))))

   ;; term
   `(term               ((t (:foreground ,fg))))
   `(term-bold          ((t (:weight bold))))
   `(term-color-black   ((t (:background ,base0 :foreground ,base0))))
   `(term-color-red     ((t (:background ,red :foreground ,red))))
   `(term-color-green   ((t (:background ,green :foreground ,green))))
   `(term-color-yellow  ((t (:background ,yellow :foreground ,yellow))))
   `(term-color-blue    ((t (:background ,blue :foreground ,blue))))
   `(term-color-magenta ((t (:background ,magenta :foreground ,magenta))))
   `(term-color-cyan    ((t (:background ,cyan :foreground ,cyan))))
   `(term-color-white   ((t (:background ,base8 :foreground ,base8))))

   ;; vterm
   `(vterm               ((t (:foreground ,fg))))
   `(vterm-color-black   ((t (:background ,(lazycat-lighten base0 0.25) :foreground ,base0))))
   `(vterm-color-red     ((t (:background ,(lazycat-lighten red 0.25) :foreground ,red))))
   `(vterm-color-green   ((t (:background ,(lazycat-lighten green 0.25) :foreground ,green))))
   `(vterm-color-yellow  ((t (:background ,(lazycat-lighten yellow 0.25) :foreground ,yellow))))
   `(vterm-color-blue    ((t (:background ,(lazycat-lighten blue 0.25) :foreground ,blue))))
   `(vterm-color-magenta ((t (:background ,(lazycat-lighten magenta 0.25) :foreground ,magenta))))
   `(vterm-color-cyan    ((t (:background ,(lazycat-lighten cyan 0.25) :foreground ,cyan))))
   `(vterm-color-white   ((t (:background ,(lazycat-lighten base8 0.25) :foreground ,base8))))

   ;; widget
   `(widget-button-pressed ((t (:foreground ,red))))
   `(widget-documentation  ((t (:foreground ,green))))

   ;; window-divider
   `(window-divider             ((t (:inherit vertical-border))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel  ((t (:inherit window-divider))))


   ;; --- plugin faces -----------------------
   ;; all-the-icons
   `(all-the-icons-red      ((t (:foreground ,red))))
   `(all-the-icons-lred     ((t (:foreground ,(lazycat-lighten red 0.3)))))
   `(all-the-icons-dred     ((t (:foreground ,(lazycat-darken red 0.3)))))
   `(all-the-icons-green    ((t (:foreground ,green))))
   `(all-the-icons-lgreen   ((t (:foreground ,(lazycat-lighten green 0.3)))))
   `(all-the-icons-dgreen   ((t (:foreground ,(lazycat-darken green 0.3)))))
   `(all-the-icons-yellow   ((t (:foreground ,yellow))))
   `(all-the-icons-lyellow  ((t (:foreground ,(lazycat-lighten yellow 0.3)))))
   `(all-the-icons-dyellow  ((t (:foreground ,(lazycat-darken yellow 0.3)))))
   `(all-the-icons-blue     ((t (:foreground ,blue))))
   `(all-the-icons-blue-alt ((t (:foreground ,teal))))
   `(all-the-icons-lblue    ((t (:foreground ,(lazycat-lighten blue 0.3)))))
   `(all-the-icons-dblue    ((t (:foreground ,dark-blue))))
   `(all-the-icons-maroon   ((t (:foreground ,magenta))))
   `(all-the-icons-lmaroon  ((t (:foreground ,(lazycat-lighten magenta 0.3)))))
   `(all-the-icons-dmaroon  ((t (:foreground ,(lazycat-darken magenta 0.3)))))
   `(all-the-icons-purple   ((t (:foreground ,violet))))
   `(all-the-icons-lpurple  ((t (:foreground ,(lazycat-lighten violet 0.3)))))
   `(all-the-icons-dpurple  ((t (:foreground ,(lazycat-darken violet 0.3)))))
   `(all-the-icons-cyan     ((t (:foreground ,cyan))))
   `(all-the-icons-cyan-alt ((t (:foreground ,cyan))))
   `(all-the-icons-lcyan    ((t (:foreground ,(lazycat-lighten cyan 0.3)))))
   `(all-the-icons-dcyan    ((t (:foreground ,dark-cyan))))
   `(all-the-icons-pink     ((t (:foreground ,(lazycat-lighten red 0.35)))))
   `(all-the-icons-lpink    ((t (:foreground ,(lazycat-lighten red 0.55)))))
   `(all-the-icons-dpink    ((t (:foreground ,red))))
   `(all-the-icons-silver   ((t (:foreground ,(lazycat-lighten grey 0.45)))))
   `(all-the-icons-lsilver  ((t (:foreground ,(lazycat-lighten grey 0.7)))))
   `(all-the-icons-dsilver  ((t (:foreground ,(lazycat-lighten grey 0.1)))))

   ;; all-the-icons-dired
   `(all-the-icons-dired-dir-face ((t (:foreground ,doc-comments))))

   ;; anzu
   `(anzu-replace-highlight ((t (:background ,base0 :foreground ,red :weight bold :strike-through t))))
   `(anzu-replace-to        ((t (:background ,base0 :foreground ,green :weight bold))))

   ;; avy
   `(avy-background-face ((t (:foreground ,comments))))
   `(avy-lead-face       ((t (:background ,highlight :foreground ,bg :distant-foreground ,fg :weight bold))))
   `(avy-lead-face-0     ((t (:inherit avy-lead-face :background ,(lazycat-lighten highlight 0.3)))))
   `(avy-lead-face-1     ((t (:inherit avy-lead-face :background ,(lazycat-lighten highlight 0.6)))))
   `(avy-lead-face-2     ((t (:inherit avy-lead-face :background ,(lazycat-lighten highlight 0.9)))))

   ;; diff-hl
   `(diff-hl-change      ((t (:foreground ,vc-modified :background ,vc-modified))))
   `(diff-hl-delete      ((t (:foreground ,vc-deleted :background ,vc-deleted))))
   `(diff-hl-insert      ((t (:foreground ,vc-added :background ,vc-added))))
   `(diff-added          ((t (:inherit hl-line :foreground ,green))))
   `(diff-changed        ((t (:foreground ,violet))))
   `(diff-context        ((t (:foreground ,(lazycat-darken fg 0.12)))))
   `(diff-removed        ((t (:foreground ,red :background ,base3))))
   `(diff-header         ((t (:foreground ,cyan :background nil))))
   `(diff-file-header    ((t (:foreground ,blue :background nil))))
   `(diff-hunk-header    ((t (:foreground ,violet))))
   `(diff-refine-added   ((t (:inherit diff-added :inverse-video t))))
   `(diff-refine-changed ((t (:inherit diff-changed :inverse-video t))))
   `(diff-refine-removed ((t (:inherit diff-removed :inverse-video t))))

   ;; flycheck
   `(flycheck-error   ((t (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,yellow)))))
   `(flycheck-info    ((t (:underline (:style wave :color ,green)))))

   ;; flycheck-posframe
   `(flycheck-posframe-face            ((t (:inherit default))))
   `(flycheck-posframe-background-face ((t (:background ,bg-alt))))
   `(flycheck-posframe-error-face      ((t (:inherit flycheck-posframe-face :foreground ,error))))
   `(flycheck-posframe-info-face       ((t (:inherit flycheck-posframe-face :foreground ,fg))))
   `(flycheck-posframe-warning-face    ((t (:inherit flycheck-posframe-face :foreground ,warning))))

   ;; flymake
   `(flymake-error   ((t (:underline (:style wave :color ,red)))))
   `(flymake-note    ((t (:underline (:style wave :color ,green)))))
   `(flymake-warning ((t (:underline (:style wave :color ,orange)))))

   ;; flyspell
   `(flyspell-incorrect ((t (:underline (:style wave :color ,error) :inherit 'unspecified))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,warning) :inherit 'unspecified))))

   ;; helpful
   `(helpful-heading ((t (:weight bold :height 1.2))))

   ;; highlight-quoted-mode
   `(highlight-quoted-symbol  ((t (:foreground ,type))))
   `(highlight-quoted-quote   ((t (:foreground ,operators))))
   `(highlight-numbers-number ((t (:inherit bold :foreground ,numbers))))

   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,fg :distant-foreground nil :weight normal))))

   ;; hl-todo
   `(hl-todo ((t (:foreground ,red :weight bold))))

   ;; hydra
   `(hydra-face-red      ((t (:foreground ,red :weight bold))))
   `(hydra-face-blue     ((t (:foreground ,blue :weight bold))))
   `(hydra-face-amaranth ((t (:foreground ,magenta :weight bold))))
   `(hydra-face-pink     ((t (:foreground ,violet :weight bold))))
   `(hydra-face-teal     ((t (:foreground ,teal :weight bold))))

   ;; iedit
   `(iedit-occurrence           ((t (:foreground ,magenta :weight bold :inverse-video t))))
   `(iedit-read-only-occurrence ((t (:inherit region))))

   ;; imenu-list
   `(imenu-list-entry-face-0 ((t (:foreground ,highlight))))
   `(imenu-list-entry-face-1 ((t (:foreground ,green))))
   `(imenu-list-entry-face-2 ((t (:foreground ,yellow))))
   `(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
   `(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
   `(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))

   ;; multiple cursors
   `(mc/cursor-face ((t (:inherit cursor))))
   `(nav-flash-face ((t (:background ,selection :foreground ,base8 :weight bold))))

   ;; neotree
   `(neo-root-dir-face    ((t (:foreground ,strings :background ,bg :box (:line-width 4 :color ,bg)))))
   `(neo-file-link-face   ((t (:foreground ,fg))))
   `(neo-dir-link-face    ((t (:foreground ,highlight))))
   `(neo-expand-btn-face  ((t (:foreground ,highlight))))
   `(neo-vc-edited-face   ((t (:foreground ,yellow))))
   `(neo-vc-added-face    ((t (:foreground ,green))))
   `(neo-vc-removed-face  ((t (:foreground ,red :strike-through t))))
   `(neo-vc-conflict-face ((t (:foreground ,magenta :weight bold))))
   `(neo-vc-ignored-face  ((t (:foreground ,comments))))

   ;; magit
   `(magit-bisect-bad        ((t (:foreground ,red))))
   `(magit-bisect-good       ((t (:foreground ,green))))
   `(magit-bisect-skip       ((t (:foreground ,orange))))
   `(magit-blame-date        ((t (:foreground ,red))))
   `(magit-blame-heading     ((t (:foreground ,orange :background ,base3 :extend t))))
   `(magit-branch-current    ((t (:foreground ,blue))))
   `(magit-branch-local      ((t (:foreground ,cyan))))
   `(magit-branch-remote     ((t (:foreground ,green))))
   `(magit-cherry-equivalent ((t (:foreground ,violet))))
   `(magit-cherry-unmatched  ((t (:foreground ,cyan))))

   `(magit-diff-added                  ((t (:foreground ,(lazycat-darken green 0.2) :background ,(lazycat-blend green bg 0.1) :extend t))))
   `(magit-diff-added-highlight        ((t (:foreground ,green :background ,(lazycat-blend green bg 0.2) :weight bold :extend t))))
   `(magit-diff-base                   ((t (:foreground ,(lazycat-darken orange 0.2) :background ,(lazycat-blend orange bg 0.1) :extend t))))
   `(magit-diff-base-highlight         ((t (:foreground ,orange :background ,(lazycat-blend orange bg 0.2) :weight bold :extend t))))
   `(magit-diff-context                ((t (:foreground ,(lazycat-darken fg 0.4) :background ,bg :extend t))))
   `(magit-diff-context-highlight      ((t (:foreground ,fg :background ,bg-alt :extend t))))
   `(magit-diff-file-heading           ((t (:foreground ,fg :weight bold :extend t))))
   `(magit-diff-file-heading-selection ((t (:foreground ,magenta :background ,dark-blue :weight bold :extend t))))
   `(magit-diff-hunk-heading           ((t (:foreground ,bg :background ,(lazycat-blend violet bg 0.3) :extend t))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,bg :background ,violet :weight bold :extend t))))
   `(magit-diff-removed                ((t (:foreground ,(lazycat-darken red 0.2) :background ,(lazycat-blend red base3 0.1) :extend t))))
   `(magit-diff-removed-highlight      ((t (:foreground ,red :background ,(lazycat-blend red base3 0.2) :weight bold :extend t))))
   `(magit-diff-lines-heading          ((t (:foreground ,yellow :background ,red :extend t))))
   `(magit-diffstat-added              ((t (:foreground ,green))))
   `(magit-diffstat-removed            ((t (:foreground ,red))))

   `(magit-dimmed      ((t (:foreground ,comments))))
   `(magit-refname     ((t (:foreground ,comments))))
   `(magit-tag         ((t (:foreground ,yellow))))
   `(magit-filename    ((t (:foreground ,violet))))
   `(magit-hash        ((t (:foreground ,comments))))
   `(magit-header-line ((t (:background ,dark-blue :foreground ,base8 :weight bold :box (:line-width 3 :color ,dark-blue)))))
   `(magit-log-author  ((t (:foreground ,orange))))
   `(magit-log-date    ((t (:foreground ,blue))))
   `(magit-log-graph   ((t (:foreground ,comments))))
   `(magit-process-ng  ((t (:inherit error))))
   `(magit-process-ok  ((t (:inherit success))))
   `(magit-section-secondary-heading ((t (:foreground ,violet :weight bold :extend t))))

   `(magit-reflog-amend       ((t (:foreground ,magenta))))
   `(magit-reflog-checkout    ((t (:foreground ,blue))))
   `(magit-reflog-cherry-pick ((t (:foreground ,green))))
   `(magit-reflog-commit      ((t (:foreground ,green))))
   `(magit-reflog-merge       ((t (:foreground ,green))))
   `(magit-reflog-other       ((t (:foreground ,cyan))))
   `(magit-reflog-rebase      ((t (:foreground ,magenta))))
   `(magit-reflog-remote      ((t (:foreground ,cyan))))
   `(magit-reflog-reset       ((t (:inherit error))))

   `(magit-section-heading           ((t (:foreground ,blue :weight bold :extend t))))
   `(magit-section-heading-selection ((t (:foreground ,orange :weight bold :extend t))))
   `(magit-section-highlight         ((t (:inherit hl-line))))

   `(magit-sequence-drop       ((t (:foreground ,red))))
   `(magit-sequence-head       ((t (:foreground ,blue))))
   `(magit-sequence-part       ((t (:foreground ,orange))))
   `(magit-sequence-stop       ((t (:foreground ,green))))
   `(magit-signature-bad       ((t (:inherit error))))
   `(magit-signature-error     ((t (:inherit error))))
   `(magit-signature-expired   ((t (:foreground ,orange))))
   `(magit-signature-good      ((t (:inherit success))))
   `(magit-signature-revoked   ((t (:foreground ,magenta))))
   `(magit-signature-untrusted ((t (:foreground ,yellow))))

   ;; mic-paren
   `(paren-face-match    ((t (:foreground ,red :background ,base0 :weight ultra-bold))))
   `(paren-face-mismatch ((t (:foreground ,base0 :background ,red :weight ultra-bold))))
   `(paren-face-no-match ((t (:inherit paren-face-mismatch :weight ultra-bold))))

   ;; parenface
   `(paren-face ((t (:foreground ,comments))))

   ;; parinfer
   `(parinfer-pretty-parens:dim-paren-face ((t (:foreground ,base5))))
   `(parinfer-smart-tab:indicator-face     ((t (:foreground ,base5))))

   ;; popup
   `(popup-face           ((t (:inherit tooltip))))
   `(popup-tip-face       ((t (:inherit popup-face :foreground ,violet :background ,base0))))
   `(popup-selection-face ((t (:background ,selection))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,magenta))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-5-face    ((t (:foreground ,violet))))
   `(rainbow-delimiters-depth-6-face    ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-7-face    ((t (:foreground ,teal))))
   `(rainbow-delimiters-unmatched-face  ((t (:foreground ,red :weight bold :inverse-video t))))
   `(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face))))

   ;; re-builder
   `(reb-match-0 ((t (:foreground ,orange :inverse-video t))))
   `(reb-match-1 ((t (:foreground ,magenta :inverse-video t))))
   `(reb-match-2 ((t (:foreground ,green :inverse-video t))))
   `(reb-match-3 ((t (:foreground ,yellow :inverse-video t))))

   ;; show-paren
   `(show-paren-match    ((t (:foreground ,red :underline t :weight normal))))
   `(show-paren-mismatch ((t (:foreground ,base0 :background ,red :weight normal))))

   ;; smartparens
   `(sp-pair-overlay-face       ((t (:background ,region))))
   `(sp-show-pair-match-face    ((t (:foreground ,red :background ,base0 :weight ultra-bold))))
   `(sp-show-pair-mismatch-face ((t (:foreground ,base0 :background ,red :weight ultra-bold))))

   ;; tabbar
   `(tabbar-default             ((t (:foreground ,bg :background ,bg :height 1.0))))
   `(tabbar-highlight           ((t (:foreground ,fg :background ,selection :distant-foreground ,bg))))
   `(tabbar-button              ((t (:foreground ,fg :background ,bg))))
   `(tabbar-button-highlight    ((t (:inherit tabbar-button :inverse-video t))))
   `(tabbar-modified            ((t (:inherit tabbar-default :foreground ,red :weight bold))))
   `(tabbar-unselected          ((t (:inherit tabbar-default :foreground ,base5))))
   `(tabbar-unselected-modified ((t (:inherit tabbar-modified))))
   `(tabbar-selected            ((t (:inherit tabbar-default :weight bold :foreground ,fg :background ,bg-alt))))
   `(tabbar-selected-modified   ((t (:inherit tabbar-selected :foreground ,green))))

   ;; treemacs
   `(treemacs-root-face          ((t (:inherit font-lock-string-face :weight bold :height 1.2))))
   `(treemacs-file-face          ((t (:foreground ,fg))))
   `(treemacs-directory-face     ((t (:foreground ,fg))))
   `(treemacs-tags-face          ((t (:foreground ,highlight))))
   `(treemacs-git-modified-face  ((t (:foreground ,violet))))
   `(treemacs-git-added-face     ((t (:foreground ,green))))
   `(treemacs-git-conflict-face  ((t (:foreground ,red))))
   `(treemacs-git-untracked-face ((t (:inherit font-lock-doc-face))))

   ;; vimish-fold
   `(vimish-fold-overlay ((t (:inherit font-lock-comment-face :background ,base0 :weight light))))
   `(vimish-fold-fringe  ((t (:foreground ,magenta))))
   `(vhl/default-face    ((t (:background ,grey))))

   ;; wgrep
   `(wgrep-face        ((t (:weight bold :foreground ,green :background ,base5))))
   `(wgrep-delete-face ((t (:foreground ,base3 :background ,red))))
   `(wgrep-done-face   ((t (:foreground ,blue))))
   `(wgrep-file-face   ((t (:foreground ,comments))))
   `(wgrep-reject-face ((t (:foreground ,red :weight bold))))

   ;; which-func
   `(which-func                           ((t (:foreground ,blue))))
   `(which-key-key-face                   ((t (:foreground ,green))))
   `(which-key-group-description-face     ((t (:foreground ,violet))))
   `(which-key-command-description-face   ((t (:foreground ,blue))))
   `(which-key-local-map-description-face ((t (:foreground ,magenta))))

   ;; whitespace
   `(whitespace-empty       ((t (:background ,base3))))
   `(whitespace-space       ((t (:foreground ,base4))))
   `(whitespace-tab         ((t (:foreground ,base4 :background ,(unless (default-value 'indent-tabs-mode) base3)))))
   `(whitespace-newline     ((t (:foreground ,base4))))
   `(whitespace-indentation ((t (:foreground ,red :background ,yellow))))
   `(whitespace-trailing    ((t (:inherit trailing-whitespace))))
   `(whitespace-line        ((t (:background ,base0 :foreground ,red :weight bold))))

   ;; yasnippet
   `(yas-field-highlight-face ((t (:inherit match))))


   ;; --- major-mode faces -------------------
   ;; elixir-mode
   `(elixir-atom-face      ((t (:foreground ,cyan))))
   `(elixir-attribute-face ((t (:foreground ,violet))))

   ;; enh-rube-mode
   `(enh-ruby-op-face                ((t (:foreground ,operators))))
   `(enh-ruby-string-delimiter-face  ((t (:inherit font-lock-string-face))))
   `(enh-ruby-heredoc-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-regexp-face            ((t (:foreground ,constants))))
   `(enh-ruby-regexp-delimiter-face  ((t (:inherit enh-ruby-regexp-face))))
   `(erm-syn-errline                 ((t (:underline (:style wave :color ,error)))))
   `(erm-syn-warnline                ((t (:underline (:style wave :color ,warning)))))

   ;; js2-mode
   `(js2-function-param    ((t (:foreground ,variables))))
   `(js2-function-call     ((t (:foreground ,functions))))
   `(js2-object-property   ((t (:foreground ,violet))))
   `(js2-jsdoc-tag         ((t (:foreground ,doc-comments))))
   `(js2-external-variable ((t (:foreground ,operators))))

   ;; makefile-*-mode
   `(makefile-targets ((t (:foreground ,blue))))

   ;; man-mode
   `(Man-overstrike ((t (:inherit bold :foreground ,operators))))
   `(Man-underline  ((t (:inherit underline :foreground ,keywords))))

   ;; markdown-mode
   `(markdown-header-face             ((t (:inherit bold :foreground ,red))))
   `(markdown-header-delimiter-face   ((t (:inherit markdown-header-face))))
   `(markdown-metadata-key-face       ((t (:foreground ,red))))
   `(markdown-list-face               ((t (:foreground ,red))))
   `(markdown-link-face               ((t (:foreground ,highlight))))
   `(markdown-url-face                ((t (:foreground ,magenta :weight normal))))
   `(markdown-italic-face             ((t (:inherit italic :foreground ,violet))))
   `(markdown-bold-face               ((t (:inherit bold :foreground ,orange))))
   `(markdown-markup-face             ((t (:foreground ,base5))))
   `(markdown-blockquote-face         ((t (:inherit italic :foreground ,doc-comments))))
   `(markdown-pre-face                ((t (:foreground ,strings))))
   `(markdown-code-face               ((t (:background ,(lazycat-lighten base3 0.05) :extend t))))
   `(markdown-reference-face          ((t (:foreground ,doc-comments))))
   `(markdown-inline-code-face        ((t (:inherit (markdown-code-face markdown-pre-face) :extend nil))))
   `(markdown-html-attr-name-face     ((t (:inherit font-lock-variable-name-face))))
   `(markdown-html-attr-value-face    ((t (:inherit font-lock-string-face))))
   `(markdown-html-entity-face        ((t (:inherit font-lock-variable-name-face))))
   `(markdown-html-tag-delimiter-face ((t (:inherit markdown-markup-face))))
   `(markdown-html-tag-name-face      ((t (:inherit font-lock-keyword-face))))

   ;; css-mode / scss-mode
   `(css-proprietary-property ((t (:foreground ,orange))))
   `(css-property             ((t (:foreground ,green))))
   `(css-selector             ((t (:foreground ,blue))))

   ;; outline
   `(outline-1 ((t (:foreground ,blue :weight bold :extend t))))
   `(outline-2 ((t (:foreground ,magenta :weight bold :extend t))))
   `(outline-3 ((t (:foreground ,violet :weight bold :extend t))))
   `(outline-4 ((t (:foreground ,(lazycat-lighten blue 0.25) :weight bold :extend t))))
   `(outline-5 ((t (:foreground ,(lazycat-lighten magenta 0.25) :weight bold :extend t))))
   `(outline-6 ((t (:foreground ,(lazycat-lighten blue 0.5) :weight bold :extend t))))
   `(outline-7 ((t (:foreground ,(lazycat-lighten magenta 0.5) :weight bold :extend t))))
   `(outline-8 ((t (:foreground ,(lazycat-lighten blue 0.8) :weight bold :extend t))))

   ;; org-mode
   `(org-archived                 ((t (:foreground ,doc-comments))))
   `(org-block                    ((t (:background ,base3 :extend t))))
   `(org-block-background         ((t (:background ,base3 :extend t))))
   `(org-block-begin-line         ((t (:foreground ,comments :background ,base3 :extend t))))
   `(org-block-end-line           ((t (:inherit org-block-begin-line))))
   `(org-checkbox                 ((t (:inherit org-todo))))
   `(org-checkbox-statistics-done ((t (:inherit org-done))))
   `(org-checkbox-statistics-todo ((t (:inherit org-todo))))
   `(org-code                     ((t (:foreground ,orange))))
   `(org-date                     ((t (:foreground ,yellow))))
   `(org-default                  ((t (:inherit variable-pitch))))
   `(org-document-info            ((t (:foreground ,builtin))))
   `(org-document-title           ((t (:foreground ,builtin :weight bold))))
   `(org-done                     ((t (:inherit org-headline-done))))
   `(org-ellipsis                 ((t (:underline nil :background nil :foreground ,grey))))
   `(org-footnote                 ((t (:foreground ,orange))))
   `(org-formula                  ((t (:foreground ,cyan))))
   `(org-headline-done            ((t (:foreground ,base5))))
   `(org-hide                     ((t (:foreground ,hidden))))

   `(org-list-dt           ((t (:foreground ,highlight))))
   `(org-meta-line         ((t (:foreground ,doc-comments))))
   `(org-priority          ((t (:foreground ,red))))
   `(org-property-value    ((t (:foreground ,doc-comments))))
   `(org-quote             ((t (:background ,base3 :slant italic :extend t))))
   `(org-special-keyword   ((t (:foreground ,doc-comments))))
   `(org-table             ((t (:foreground ,violet))))
   `(org-tag               ((t (:foreground ,doc-comments :weight normal))))
   `(org-ref-cite-face     ((t (:foreground ,yellow :weight light :underline t))))
   `(org-latex-and-related ((t (:foreground ,base8 :weight bold))))
   `(org-todo              ((t (:foreground ,green))))
   `(org-verbatim          ((t (:foreground ,green))))
   `(org-warning           ((t (:foreground ,warning))))

   ;; org-agenda
   `(org-agenda-done               ((t (:inherit org-done))))
   `(org-agenda-dimmed-todo-face   ((t (:foreground ,comments))))
   `(org-agenda-date               ((t (:foreground ,violet :weight ultra-bold))))
   `(org-agenda-date-today         ((t (:foreground ,(lazycat-lighten violet 0.4) :weight ultra-bold))))
   `(org-agenda-date-weekend       ((t (:foreground ,(lazycat-darken violet 0.4) :weight ultra-bold))))
   `(org-agenda-structure          ((t (:foreground ,fg :weight ultra-bold))))
   `(org-agenda-clocking           ((t (:background ,(lazycat-blend blue bg 0.2)))))
   `(org-upcoming-deadline         ((t (:foreground ,(lazycat-blend fg bg 0.8)))))
   `(org-upcoming-distant-deadline ((t (:foreground ,(lazycat-blend fg bg 0.5)))))
   `(org-scheduled                 ((t (:foreground ,fg))))
   `(org-scheduled-today           ((t (:foreground ,base7))))
   `(org-scheduled-previously      ((t (:foreground ,base8))))
   `(org-time-grid                 ((t (:foreground ,comments))))
   `(org-sexp-date                 ((t (:foreground ,fg))))

   ;; typescript-mode
   `(typescript-jsdoc-tag   ((t (:foreground ,doc-comments))))
   `(typescript-jsdoc-type  ((t (:foreground ,(lazycat-darken doc-comments 0.15)))))
   `(typescript-jsdoc-value ((t (:foreground ,(lazycat-lighten doc-comments 0.15)))))

   ;; sh-mode
   `(sh-heredoc     ((t (:inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))

   ;; web-mode
   `(web-mode-doctype-face          ((t (:foreground ,comments))))
   `(web-mode-html-tag-face         ((t (:foreground ,methods))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,methods))))
   `(web-mode-html-attr-name-face   ((t (:foreground ,type))))
   `(web-mode-html-entity-face      ((t (:foreground ,cyan :inherit italic))))
   `(web-mode-block-control-face    ((t (:foreground ,orange))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,operators))))

   ;; woman
   `(woman-bold   ((t (:inherit Man-overstrike))))
   `(woman-italic ((t (:inherit Man-underline)))))

  (custom-theme-set-variables
   'lazycat
   `(ansi-color-names-vector [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])
   `(rustic-ansi-faces [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])
   `(vc-annotate-color-map
     (list (cons 20  ,green)
           (cons 40  ,(lazycat-blend yellow green (/ 1.0 3)))
           (cons 60  ,(lazycat-blend yellow green (/ 2.0 3)))
           (cons 80  ,yellow)
           (cons 100 ,(lazycat-blend orange yellow (/ 1.0 3)))
           (cons 120 ,(lazycat-blend orange yellow (/ 2.0 3)))
           (cons 140 ,orange)
           (cons 160 ,(lazycat-blend magenta orange (/ 1.0 3)))
           (cons 180 ,(lazycat-blend magenta orange (/ 2.0 3)))
           (cons 200 ,magenta)
           (cons 220 ,(lazycat-blend red magenta (/ 1.0 3)))
           (cons 240 ,(lazycat-blend red magenta (/ 2.0 3)))
           (cons 260 ,red)
           (cons 280 ,(lazycat-blend grey red (/ 1.0 4)))
           (cons 300 ,(lazycat-blend grey red (/ 2.0 4)))
           (cons 320 ,(lazycat-blend grey red (/ 3.0 4)))
           (cons 340 ,base5)
           (cons 360 ,base5)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background ,bg)))


;; (set-face-bold 'bold nil)
;; (set-face-italic 'italic nil)

(provide-theme 'lazycat)
