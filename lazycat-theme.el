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
  (if (listp color)
      (cl-loop for c in color collect (lazycat-darken c alpha))
    (lazycat-blend color "#000000" (- 1 alpha))))

;;;###autoload
(defun lazycat-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (if (listp color)
      (cl-loop for c in color collect (lazycat-lighten c alpha))
    (lazycat-blend color "#FFFFFF" (- 1 alpha))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(let* ((lazycat-background      "#242525")
       (lazycat-background-alt  "#333333")
       (lazycat-base0           "#1B2229")
       (lazycat-base1           "#1C1F24")
       (lazycat-base2           "#202328")
       (lazycat-base3           "#23272E")
       (lazycat-base4           "#3F444A")
       (lazycat-base5           "#5B6268")
       (lazycat-base6           "#73797E")
       (lazycat-base7           "#9CA0A4")
       (lazycat-base8           "#DFDFDF")
       (lazycat-foreground      "#00CE00")
       (lazycat-foreground-alt  "#008B00") ;; green4
       ;; base colors
       (lazycat-grey            lazycat-base4)
       (lazycat-red             "#FF6C6B") ;; 珊瑚红 error,deleted,invalid,removed,mismatch
       (lazycat-orange          "#DA8548") ;; 焦糖橙 warning,modified,mark
       (lazycat-green           "#98BE65") ;; 橄榄绿 success,added,executable
       (lazycat-teal            "#4DB5BD") ;; 浅水鸭
       (lazycat-yellow          "#ECBE7B") ;; 沙黄 warning
       (lazycat-blue            "#51AFEF")
       (lazycat-magenta         "#C678DD") ;; 洋红
       (lazycat-violet          "#A9A1E1") ;; 紫罗兰
       (lazycat-cyan            "#46D9FF") ;; method,symlink
       (lazycat-gold            "#EEC900") ;; gold2 function,variable
       ;; special handling colors
       (lazycat-blue-d          "#2257A0") ;; 皇室蓝 lazy-highlight,magit-header-line
       (lazycat-blue-l          "#00B8FF") ;; builtin, type
       (lazycat-cyan-d          "#5699AF")
       ;; face categories
       (lazycat-comments        "#A7A7A7")
       (lazycat-doc-comments    "#AAAAAA")
       (lazycat-highlight       "#00FF00") ;; green
       (lazycat-keyword         "#165EFF")
       (lazycat-string          "#DFD67A")
       (lazycat-constant        "#BD00FF")
       (lazycat-operator        "#00CDCD") ;; cyan3
       (lazycat-region          "#3F90F7")
       (lazycat-region-fg       "#FFFFFF") ;; white
       (lazycat-selection       lazycat-blue-d)
       (lazycat-builtin         lazycat-blue-l)
       (lazycat-function        lazycat-gold)
       (lazycat-method          lazycat-cyan)
       (lazycat-type            lazycat-blue-l)
       (lazycat-variable        lazycat-gold)
       (lazycat-success         lazycat-green)
       (lazycat-warning         lazycat-yellow)
       (lazycat-error           lazycat-red)
       ;; vc
       (lazycat-diff-added      lazycat-green)
       (lazycat-diff-modified   lazycat-orange)
       (lazycat-diff-deleted    lazycat-red)
       ;; custom categories
       (lazycat-hidden          lazycat-background)
       (bold                    nil)
       (italic                  nil))

  (custom-theme-set-faces
   'lazycat

   ;; --- base faces -------------------------
   `(bold    ((t (:weight bold :foreground ,(unless bold lazycat-base8)))))
   '(italic  ((t (:slant italic))))

   `(default ((t (:background ,lazycat-background :foreground ,lazycat-foreground))))
   `(fringe  ((t (:inherit default :foreground ,lazycat-base4))))
   `(region              ((t (:background ,lazycat-region :foreground ,lazycat-region-fg))))
   `(highlight           ((t (:background ,lazycat-highlight :foreground ,lazycat-base0 :distant-foreground ,lazycat-base8))))
   `(cursor              ((t (:background ,lazycat-highlight))))
   `(shadow              ((t (:foreground ,lazycat-base5))))
   `(minibuffer-prompt   ((t (:foreground ,lazycat-highlight))))
   `(tooltip             ((t (:background ,lazycat-base3 :foreground ,lazycat-foreground))))
   `(secondary-selection ((t (:background ,lazycat-grey :extend t))))
   `(lazy-highlight      ((t (:background ,lazycat-blue-d :foreground ,lazycat-base8 :distant-foreground ,lazycat-base0 :weight bold))))
   `(match               ((t (:foreground ,lazycat-green :background ,lazycat-base0 :weight bold))))
   `(trailing-whitespace ((t (:background ,lazycat-red))))
   `(nobreak-space       ((t (:inherit default :underline nil))))
   `(vertical-border     ((t (:background ,(lazycat-darken lazycat-base1 0.1)
                                          :foreground ,(lazycat-darken lazycat-base1 0.1)))))
   `(link                ((t (:foreground ,lazycat-highlight :underline t))))

   `(error   ((t (:foreground ,lazycat-error))))
   `(warning ((t (:foreground ,lazycat-warning))))
   `(success ((t (:foreground ,lazycat-success))))

   `(font-lock-builtin-face              ((t (:foreground ,lazycat-builtin))))
   `(font-lock-comment-face              ((t (:foreground ,lazycat-comments))))
   `(font-lock-comment-delimiter-face    ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face                  ((t (:inherit font-lock-comment-face :foreground ,lazycat-doc-comments))))
   `(font-lock-constant-face             ((t (:foreground ,lazycat-constant))))
   `(font-lock-function-name-face        ((t (:foreground ,lazycat-function))))
   `(font-lock-keyword-face              ((t (:foreground ,lazycat-keyword))))
   `(font-lock-string-face               ((t (:foreground ,lazycat-string))))
   `(font-lock-type-face                 ((t (:foreground ,lazycat-type))))
   `(font-lock-variable-name-face        ((t (:foreground ,lazycat-variable))))
   `(font-lock-warning-face              ((t (:inherit warning))))
   `(font-lock-negation-char-face        ((t (:inherit bold :foreground ,lazycat-operator))))
   `(font-lock-preprocessor-face         ((t (:inherit bold :foreground ,lazycat-operator))))
   `(font-lock-preprocessor-char-face    ((t (:inherit 'bold :foreground ,lazycat-operator))))
   `(font-lock-regexp-grouping-backslash ((t (:inherit bold :foreground ,lazycat-operator))))
   `(font-lock-regexp-grouping-construct ((t (:inherit bold :foreground ,lazycat-operator))))

   ;; mode-line
   `(mode-line           ((t (:background ,lazycat-background :foreground ,lazycat-background-alt))))
   `(mode-line-inactive  ((t (:background ,lazycat-background :foreground ,lazycat-foreground))))
   `(mode-line-emphasis  ((t (:foreground ,lazycat-highlight :distant-foreground ,lazycat-background :height 0.1))))
   `(mode-line-highlight ((t (:inherit highlight :distant-foreground ,lazycat-background :height 0.1))))
   `(mode-line-buffer-id ((t (:weight bold :height 0.1))))

   ;; header-line
   `(header-line ((t (:inherit default :height ,(face-attribute 'default :height)))))

   ;; 1. Line number faces must explicitly disable its text style attributes
   ;;    because nearby faces may "bleed" into the line numbers otherwise.
   ;; 2. All other line number plugin faces should &inherit from these.
   `(line-number
     ((t (:inherit default
                   :foreground ,lazycat-base5 :distant-foreground nil
                   :weight normal :italic nil :underline nil :strike-through nil))))
   `(line-number-current-line
     ((t (:inherit (hl-line default)
                   :foreground ,lazycat-foreground :distant-foreground nil
                   :weight normal :italic nil :underline nil :strike-through nil))))


   ;; --- built-in plugin faces --------------
   ;; cperl
   `(cperl-array-face          ((t (:weight bold :inherit font-lock-variable-name-face))))
   `(cperl-hash-face           ((t (:weight bold :slant italic :inherit font-lock-variable-name-face))))
   `(cperl-nonoverridable-face ((t (:inherit font-lock-builtin-face))))

   ;; compilation
   `(compilation-column-number  ((t (:inherit font-lock-comment-face))))
   `(compilation-line-number    ((t (:foreground ,lazycat-highlight))))
   `(compilation-error          ((t (:inherit error :weight bold))))
   `(compilation-warning        ((t (:inherit warning :slant italic))))
   `(compilation-info           ((t (:inherit success))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info))))
   `(compilation-mode-line-fail ((t (:inherit compilation-error))))

   ;; custom
   `(custom-variable-button   ((t (:foreground ,lazycat-green :underline t))))
   `(custom-saved             ((t (:foreground ,lazycat-green :background ,(lazycat-blend lazycat-green lazycat-background 0.2) :bold bold))))
   `(custom-comment           ((t (:foreground ,lazycat-foreground :background ,lazycat-region))))
   `(custom-comment-tag       ((t (:foreground ,lazycat-grey))))
   `(custom-modified          ((t (:foreground ,lazycat-blue :background ,(lazycat-blend lazycat-blue lazycat-background 0.2)))))
   `(custom-variable-tag      ((t (:foreground ,lazycat-magenta))))
   `(custom-visibility        ((t (:foreground ,lazycat-blue :underline nil))))
   `(custom-group-subtitle    ((t (:foreground ,lazycat-red))))
   `(custom-group-tag         ((t (:foreground ,lazycat-violet))))
   `(custom-group-tag-1       ((t (:foreground ,lazycat-blue))))
   `(custom-set               ((t (:foreground ,lazycat-yellow :background ,lazycat-background))))
   `(custom-themed            ((t (:foreground ,lazycat-yellow :background ,lazycat-background))))
   `(custom-invalid           ((t (:foreground ,lazycat-red :background ,(lazycat-blend lazycat-red lazycat-background 0.2)))))
   `(custom-variable-obsolete ((t (:foreground ,lazycat-grey :background ,lazycat-background))))
   `(custom-state             ((t (:foreground ,lazycat-green :background ,(lazycat-blend lazycat-green lazycat-background 0.2)))))
   `(custom-changed           ((t (:foreground ,lazycat-blue :background ,lazycat-background))))

   ;; dired
   `(dired-directory  ((t (:foreground ,lazycat-builtin))))
   `(dired-ignored    ((t (:foreground ,lazycat-comments))))
   `(dired-flagged    ((t (:foreground ,lazycat-red))))
   `(dired-header     ((t (:foreground ,lazycat-variable :weight bold))))
   `(dired-mark       ((t (:foreground ,lazycat-orange :weight bold))))
   `(dired-marked     ((t (:foreground ,lazycat-magenta :weight bold :inverse-video t))))
   `(dired-perm-write ((t (:foreground ,lazycat-foreground :underline t))))
   `(dired-symlink    ((t (:foreground ,lazycat-cyan :weight bold))))
   `(dired-warning    ((t (:foreground ,lazycat-warning))))

   ;; ediff
   `(ediff-fine-diff-A    ((t (:background ,(lazycat-blend lazycat-selection lazycat-background 0.7) :weight bold :extend t))))
   `(ediff-fine-diff-B    ((t (:inherit ediff-fine-diff-A))))
   `(ediff-fine-diff-C    ((t (:inherit ediff-fine-diff-A))))
   `(ediff-current-diff-A ((t (:background ,(lazycat-blend lazycat-selection lazycat-background 0.3) :extend t))))
   `(ediff-current-diff-B ((t (:inherit ediff-current-diff-A))))
   `(ediff-current-diff-C ((t (:inherit ediff-current-diff-A))))
   `(ediff-even-diff-A    ((t (:inherit hl-line))))
   `(ediff-even-diff-B    ((t (:inherit ediff-even-diff-A))))
   `(ediff-even-diff-C    ((t (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-A     ((t (:inherit ediff-even-diff-A))))
   `(ediff-odd-diff-B     ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-C     ((t (:inherit ediff-odd-diff-A))))

   ;; eshell
   `(eshell-prompt        ((t (:foreground ,lazycat-highlight :weight bold))))
   `(eshell-ls-archive    ((t (:foreground ,lazycat-magenta))))
   `(eshell-ls-backup     ((t (:foreground ,lazycat-yellow))))
   `(eshell-ls-clutter    ((t (:foreground ,lazycat-red))))
   `(eshell-ls-directory  ((t (:foreground ,lazycat-blue))))
   `(eshell-ls-executable ((t (:foreground ,lazycat-green))))
   `(eshell-ls-missing    ((t (:foreground ,lazycat-red))))
   `(eshell-ls-product    ((t (:foreground ,lazycat-orange))))
   `(eshell-ls-readonly   ((t (:foreground ,lazycat-orange))))
   `(eshell-ls-special    ((t (:foreground ,lazycat-violet))))
   `(eshell-ls-symlink    ((t (:foreground ,lazycat-cyan))))
   `(eshell-ls-unreadable ((t (:foreground ,lazycat-base5))))

   ;; fix-ido
   `(flx-highlight-face ((t (:weight bold :foreground ,lazycat-yellow :underline nil))))

   ;; hi-lock
   `(hi-yellow  ((t (:background ,lazycat-yellow))))
   `(hi-pink    ((t (:background ,lazycat-magenta))))
   `(hi-red-b   ((t (:foreground ,lazycat-red :weight bold))))
   `(hi-green   ((t (:background ,lazycat-green))))
   `(hi-green-b ((t (:foreground ,lazycat-green :weight bold))))
   `(hi-blue    ((t (:background ,lazycat-blue))))
   `(hi-blue-b  ((t (:foreground ,lazycat-blue :weight bold))))

   ;; hl-line
   `(hl-line ((t (:background ,lazycat-background-alt :extend t))))

   ;; ido
   `(ido-first-match ((t (:foreground ,lazycat-orange))))
   `(ido-indicator   ((t (:foreground ,lazycat-red :background ,lazycat-background))))
   `(ido-only-match  ((t (:foreground ,lazycat-green))))
   `(ido-subdir      ((t (:foreground ,lazycat-violet))))
   `(ido-virtual     ((t (:foreground ,lazycat-comments))))

   ;; isearch
   `(isearch      ((t (:inherit lazy-highlight :weight bold))))
   `(isearch-fail ((t (:background ,lazycat-error :foreground ,lazycat-base0 :weight bold))))

   ;; linum totally inherit line-number
   `(linum ((t (:inherit default
                         :foreground ,lazycat-base5 :distant-foreground nil
                         :weight normal :italic nil
                         :underline nil :strike-through nil))))

   ;; message
   `(message-header-name    ((t (:foreground ,lazycat-green))))
   `(message-header-subject ((t (:foreground ,lazycat-highlight :weight bold))))
   `(message-header-to      ((t (:foreground ,lazycat-highlight :weight bold))))
   `(message-header-cc      ((t (:inherit message-header-to :foreground ,(lazycat-darken lazycat-highlight 0.15)))))
   `(message-header-other   ((t (:foreground ,lazycat-violet))))
   `(message-header-newsgroups ((t (:foreground ,lazycat-yellow))))
   `(message-header-xheader ((t (:foreground ,lazycat-doc-comments))))
   `(message-separator      ((t (:foreground ,lazycat-comments))))
   `(message-mml            ((t (:foreground ,lazycat-comments :slant italic))))
   `(message-cited-text     ((t (:foreground ,lazycat-magenta))))

   ;; term
   `(term               ((t (:foreground ,lazycat-foreground))))
   `(term-bold          ((t (:weight bold))))
   `(term-color-black   ((t (:background ,lazycat-base0 :foreground ,lazycat-base0))))
   `(term-color-red     ((t (:background ,lazycat-red :foreground ,lazycat-red))))
   `(term-color-green   ((t (:background ,lazycat-green :foreground ,lazycat-green))))
   `(term-color-yellow  ((t (:background ,lazycat-yellow :foreground ,lazycat-yellow))))
   `(term-color-blue    ((t (:background ,lazycat-blue :foreground ,lazycat-blue))))
   `(term-color-magenta ((t (:background ,lazycat-magenta :foreground ,lazycat-magenta))))
   `(term-color-cyan    ((t (:background ,lazycat-cyan :foreground ,lazycat-cyan))))
   `(term-color-white   ((t (:background ,lazycat-base8 :foreground ,lazycat-base8))))

   ;; vterm
   `(vterm               ((t (:foreground ,lazycat-foreground))))
   `(vterm-color-black   ((t (:background ,(lazycat-lighten lazycat-base0 0.25) :foreground ,lazycat-base0))))
   `(vterm-color-red     ((t (:background ,(lazycat-lighten lazycat-red 0.25) :foreground ,lazycat-red))))
   `(vterm-color-green   ((t (:background ,(lazycat-lighten lazycat-green 0.25) :foreground ,lazycat-green))))
   `(vterm-color-yellow  ((t (:background ,(lazycat-lighten lazycat-yellow 0.25) :foreground ,lazycat-yellow))))
   `(vterm-color-blue    ((t (:background ,(lazycat-lighten lazycat-blue 0.25) :foreground ,lazycat-blue))))
   `(vterm-color-magenta ((t (:background ,(lazycat-lighten lazycat-magenta 0.25) :foreground ,lazycat-magenta))))
   `(vterm-color-cyan    ((t (:background ,(lazycat-lighten lazycat-cyan 0.25) :foreground ,lazycat-cyan))))
   `(vterm-color-white   ((t (:background ,(lazycat-lighten lazycat-base8 0.25) :foreground ,lazycat-base8))))

   ;; widget
   `(widget-button-pressed ((t (:foreground ,lazycat-red))))
   `(widget-documentation  ((t (:foreground ,lazycat-green))))

   ;; window-divider
   `(window-divider             ((t (:inherit vertical-border))))
   `(window-divider-first-pixel ((t (:inherit window-divider))))
   `(window-divider-last-pixel  ((t (:inherit window-divider))))


   ;; --- plugin faces -----------------------
   ;; all-the-icons
   `(all-the-icons-red      ((t (:foreground ,lazycat-red))))
   `(all-the-icons-lred     ((t (:foreground ,(lazycat-lighten lazycat-red 0.3)))))
   `(all-the-icons-dred     ((t (:foreground ,(lazycat-darken lazycat-red 0.3)))))
   `(all-the-icons-green    ((t (:foreground ,lazycat-green))))
   `(all-the-icons-lgreen   ((t (:foreground ,(lazycat-lighten lazycat-green 0.3)))))
   `(all-the-icons-dgreen   ((t (:foreground ,(lazycat-darken lazycat-green 0.3)))))
   `(all-the-icons-yellow   ((t (:foreground ,lazycat-yellow))))
   `(all-the-icons-lyellow  ((t (:foreground ,(lazycat-lighten lazycat-yellow 0.3)))))
   `(all-the-icons-dyellow  ((t (:foreground ,(lazycat-darken lazycat-yellow 0.3)))))
   `(all-the-icons-blue     ((t (:foreground ,lazycat-blue))))
   `(all-the-icons-blue-alt ((t (:foreground ,lazycat-teal))))
   `(all-the-icons-lblue    ((t (:foreground ,(lazycat-lighten lazycat-blue 0.3)))))
   `(all-the-icons-dblue    ((t (:foreground ,lazycat-blue-d))))
   `(all-the-icons-maroon   ((t (:foreground ,lazycat-magenta))))
   `(all-the-icons-lmaroon  ((t (:foreground ,(lazycat-lighten lazycat-magenta 0.3)))))
   `(all-the-icons-dmaroon  ((t (:foreground ,(lazycat-darken lazycat-magenta 0.3)))))
   `(all-the-icons-purple   ((t (:foreground ,lazycat-violet))))
   `(all-the-icons-lpurple  ((t (:foreground ,(lazycat-lighten lazycat-violet 0.3)))))
   `(all-the-icons-dpurple  ((t (:foreground ,(lazycat-darken lazycat-violet 0.3)))))
   `(all-the-icons-cyan     ((t (:foreground ,lazycat-cyan))))
   `(all-the-icons-cyan-alt ((t (:foreground ,lazycat-cyan))))
   `(all-the-icons-lcyan    ((t (:foreground ,(lazycat-lighten lazycat-cyan 0.3)))))
   `(all-the-icons-dcyan    ((t (:foreground ,lazycat-cyan-d))))
   `(all-the-icons-pink     ((t (:foreground ,(lazycat-lighten lazycat-red 0.35)))))
   `(all-the-icons-lpink    ((t (:foreground ,(lazycat-lighten lazycat-red 0.55)))))
   `(all-the-icons-dpink    ((t (:foreground ,lazycat-red))))
   `(all-the-icons-silver   ((t (:foreground ,(lazycat-lighten lazycat-grey 0.45)))))
   `(all-the-icons-lsilver  ((t (:foreground ,(lazycat-lighten lazycat-grey 0.7)))))
   `(all-the-icons-dsilver  ((t (:foreground ,(lazycat-lighten lazycat-grey 0.1)))))

   ;; anzu
   `(anzu-replace-highlight ((t (:background ,lazycat-base0 :foreground ,lazycat-red :weight bold :strike-through t))))
   `(anzu-replace-to        ((t (:background ,lazycat-base0 :foreground ,lazycat-green :weight bold))))

   ;; avy
   `(avy-background-face ((t (:foreground ,lazycat-comments))))
   `(avy-lead-face       ((t (:background ,lazycat-highlight :foreground ,lazycat-background :distant-foreground ,lazycat-foreground :weight bold))))
   `(avy-lead-face-0     ((t (:inherit avy-lead-face :background ,(lazycat-lighten lazycat-highlight 0.3)))))
   `(avy-lead-face-1     ((t (:inherit avy-lead-face :background ,(lazycat-lighten lazycat-highlight 0.6)))))
   `(avy-lead-face-2     ((t (:inherit avy-lead-face :background ,(lazycat-lighten lazycat-highlight 0.9)))))

   ;; diff-hl
   `(diff-hl-change      ((t (:foreground ,lazycat-diff-modified :background ,lazycat-diff-modified))))
   `(diff-hl-delete      ((t (:foreground ,lazycat-diff-deleted :background ,lazycat-diff-deleted))))
   `(diff-hl-insert      ((t (:foreground ,lazycat-diff-added :background ,lazycat-diff-added))))
   `(diff-added          ((t (:inherit hl-line :foreground ,lazycat-green))))
   `(diff-changed        ((t (:foreground ,lazycat-violet))))
   `(diff-context        ((t (:foreground ,(lazycat-darken lazycat-foreground 0.12)))))
   `(diff-removed        ((t (:foreground ,lazycat-red :background ,lazycat-base3))))
   `(diff-header         ((t (:foreground ,lazycat-cyan :background nil))))
   `(diff-file-header    ((t (:foreground ,lazycat-blue :background nil))))
   `(diff-hunk-header    ((t (:foreground ,lazycat-violet))))
   `(diff-refine-added   ((t (:inherit diff-added :inverse-video t))))
   `(diff-refine-changed ((t (:inherit diff-changed :inverse-video t))))
   `(diff-refine-removed ((t (:inherit diff-removed :inverse-video t))))

   ;; flycheck
   `(flycheck-error   ((t (:underline (:style wave :color ,lazycat-red)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,lazycat-yellow)))))
   `(flycheck-info    ((t (:underline (:style wave :color ,lazycat-green)))))

   ;; flycheck-posframe
   `(flycheck-posframe-face            ((t (:inherit default))))
   `(flycheck-posframe-background-face ((t (:background ,lazycat-background-alt))))
   `(flycheck-posframe-error-face      ((t (:inherit flycheck-posframe-face :foreground ,lazycat-error))))
   `(flycheck-posframe-info-face       ((t (:inherit flycheck-posframe-face :foreground ,lazycat-foreground))))
   `(flycheck-posframe-warning-face    ((t (:inherit flycheck-posframe-face :foreground ,lazycat-warning))))

   ;; flymake
   `(flymake-error   ((t (:underline (:style wave :color ,lazycat-red)))))
   `(flymake-note    ((t (:underline (:style wave :color ,lazycat-green)))))
   `(flymake-warning ((t (:underline (:style wave :color ,lazycat-orange)))))

   ;; flyspell
   `(flyspell-incorrect ((t (:underline (:style wave :color ,lazycat-error) :inherit 'unspecified))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,lazycat-warning) :inherit 'unspecified))))

   ;; helpful
   `(helpful-heading ((t (:weight bold :height 1.2))))

   ;; highlight-quoted-mode
   `(highlight-quoted-symbol  ((t (:foreground ,lazycat-type))))
   `(highlight-quoted-quote   ((t (:foreground ,lazycat-operator))))
   `(highlight-numbers-number ((t (:inherit bold :foreground ,lazycat-orange))))

   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,lazycat-foreground :distant-foreground nil :weight normal))))

   ;; hl-todo
   `(hl-todo ((t (:foreground ,lazycat-red :weight bold))))

   ;; hydra
   `(hydra-face-red      ((t (:foreground ,lazycat-red :weight bold))))
   `(hydra-face-blue     ((t (:foreground ,lazycat-blue :weight bold))))
   `(hydra-face-amaranth ((t (:foreground ,lazycat-magenta :weight bold))))
   `(hydra-face-pink     ((t (:foreground ,lazycat-violet :weight bold))))
   `(hydra-face-teal     ((t (:foreground ,lazycat-teal :weight bold))))

   ;; iedit
   `(iedit-occurrence           ((t (:foreground ,lazycat-magenta :weight bold :inverse-video t))))
   `(iedit-read-only-occurrence ((t (:inherit region))))

   ;; imenu-list
   `(imenu-list-entry-face-0 ((t (:foreground ,lazycat-highlight))))
   `(imenu-list-entry-face-1 ((t (:foreground ,lazycat-green))))
   `(imenu-list-entry-face-2 ((t (:foreground ,lazycat-yellow))))
   `(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
   `(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
   `(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))

   ;; multiple cursors
   `(mc/cursor-face ((t (:inherit cursor))))
   `(nav-flash-face ((t (:background ,lazycat-selection :foreground ,lazycat-base8 :weight bold))))

   ;; neotree
   `(neo-root-dir-face    ((t (:foreground ,lazycat-string :background ,lazycat-background :box (:line-width 4 :color ,lazycat-background)))))
   `(neo-file-link-face   ((t (:foreground ,lazycat-foreground))))
   `(neo-dir-link-face    ((t (:foreground ,lazycat-highlight))))
   `(neo-expand-btn-face  ((t (:foreground ,lazycat-highlight))))
   `(neo-vc-edited-face   ((t (:foreground ,lazycat-yellow))))
   `(neo-vc-added-face    ((t (:foreground ,lazycat-green))))
   `(neo-vc-removed-face  ((t (:foreground ,lazycat-red :strike-through t))))
   `(neo-vc-conflict-face ((t (:foreground ,lazycat-magenta :weight bold))))
   `(neo-vc-ignored-face  ((t (:foreground ,lazycat-comments))))

   ;; magit
   `(magit-bisect-bad        ((t (:foreground ,lazycat-red))))
   `(magit-bisect-good       ((t (:foreground ,lazycat-green))))
   `(magit-bisect-skip       ((t (:foreground ,lazycat-orange))))
   `(magit-blame-date        ((t (:foreground ,lazycat-red))))
   `(magit-blame-heading     ((t (:foreground ,lazycat-orange :background ,lazycat-base3 :extend t))))
   `(magit-branch-current    ((t (:foreground ,lazycat-blue))))
   `(magit-branch-local      ((t (:foreground ,lazycat-cyan))))
   `(magit-branch-remote     ((t (:foreground ,lazycat-green))))
   `(magit-cherry-equivalent ((t (:foreground ,lazycat-violet))))
   `(magit-cherry-unmatched  ((t (:foreground ,lazycat-cyan))))

   `(magit-diff-added                  ((t (:foreground ,(lazycat-darken lazycat-green 0.2) :background ,(lazycat-blend lazycat-green lazycat-background 0.1) :extend t))))
   `(magit-diff-added-highlight        ((t (:foreground ,lazycat-green :background ,(lazycat-blend lazycat-green lazycat-background 0.2) :weight bold :extend t))))
   `(magit-diff-base                   ((t (:foreground ,(lazycat-darken lazycat-orange 0.2) :background ,(lazycat-blend lazycat-orange lazycat-background 0.1) :extend t))))
   `(magit-diff-base-highlight         ((t (:foreground ,lazycat-orange :background ,(lazycat-blend lazycat-orange lazycat-background 0.2) :weight bold :extend t))))
   `(magit-diff-context                ((t (:foreground ,(lazycat-darken lazycat-foreground 0.4) :background ,lazycat-background :extend t))))
   `(magit-diff-context-highlight      ((t (:foreground ,lazycat-foreground :background ,lazycat-background-alt :extend t))))
   `(magit-diff-file-heading           ((t (:foreground ,lazycat-foreground :weight bold :extend t))))
   `(magit-diff-file-heading-selection ((t (:foreground ,lazycat-magenta :background ,lazycat-blue-d :weight bold :extend t))))
   `(magit-diff-hunk-heading           ((t (:foreground ,lazycat-background :background ,(lazycat-blend lazycat-violet lazycat-background 0.3) :extend t))))
   `(magit-diff-hunk-heading-highlight ((t (:foreground ,lazycat-background :background ,lazycat-violet :weight bold :extend t))))
   `(magit-diff-removed                ((t (:foreground ,(lazycat-darken lazycat-red 0.2) :background ,(lazycat-blend lazycat-red lazycat-base3 0.1) :extend t))))
   `(magit-diff-removed-highlight      ((t (:foreground ,lazycat-red :background ,(lazycat-blend lazycat-red lazycat-base3 0.2) :weight bold :extend t))))
   `(magit-diff-lines-heading          ((t (:foreground ,lazycat-yellow :background ,lazycat-red :extend t))))
   `(magit-diffstat-added              ((t (:foreground ,lazycat-green))))
   `(magit-diffstat-removed            ((t (:foreground ,lazycat-red))))

   `(magit-dimmed      ((t (:foreground ,lazycat-comments))))
   `(magit-refname     ((t (:foreground ,lazycat-comments))))
   `(magit-tag         ((t (:foreground ,lazycat-yellow))))
   `(magit-filename    ((t (:foreground ,lazycat-violet))))
   `(magit-hash        ((t (:foreground ,lazycat-comments))))
   `(magit-header-line ((t (:background ,lazycat-blue-d :foreground ,lazycat-base8 :weight bold :box (:line-width 3 :color ,lazycat-blue-d)))))
   `(magit-log-author  ((t (:foreground ,lazycat-orange))))
   `(magit-log-date    ((t (:foreground ,lazycat-blue))))
   `(magit-log-graph   ((t (:foreground ,lazycat-comments))))
   `(magit-process-ng  ((t (:inherit error))))
   `(magit-process-ok  ((t (:inherit success))))
   `(magit-section-secondary-heading ((t (:foreground ,lazycat-violet :weight bold :extend t))))

   `(magit-reflog-amend       ((t (:foreground ,lazycat-magenta))))
   `(magit-reflog-checkout    ((t (:foreground ,lazycat-blue))))
   `(magit-reflog-cherry-pick ((t (:foreground ,lazycat-green))))
   `(magit-reflog-commit      ((t (:foreground ,lazycat-green))))
   `(magit-reflog-merge       ((t (:foreground ,lazycat-green))))
   `(magit-reflog-other       ((t (:foreground ,lazycat-cyan))))
   `(magit-reflog-rebase      ((t (:foreground ,lazycat-magenta))))
   `(magit-reflog-remote      ((t (:foreground ,lazycat-cyan))))
   `(magit-reflog-reset       ((t (:inherit error))))

   `(magit-section-heading           ((t (:foreground ,lazycat-blue :weight bold :extend t))))
   `(magit-section-heading-selection ((t (:foreground ,lazycat-orange :weight bold :extend t))))
   `(magit-section-highlight         ((t (:inherit hl-line))))

   `(magit-sequence-drop       ((t (:foreground ,lazycat-red))))
   `(magit-sequence-head       ((t (:foreground ,lazycat-blue))))
   `(magit-sequence-part       ((t (:foreground ,lazycat-orange))))
   `(magit-sequence-stop       ((t (:foreground ,lazycat-green))))
   `(magit-signature-bad       ((t (:inherit error))))
   `(magit-signature-error     ((t (:inherit error))))
   `(magit-signature-expired   ((t (:foreground ,lazycat-orange))))
   `(magit-signature-good      ((t (:inherit success))))
   `(magit-signature-revoked   ((t (:foreground ,lazycat-magenta))))
   `(magit-signature-untrusted ((t (:foreground ,lazycat-yellow))))

   ;; mic-paren
   `(paren-face-match    ((t (:foreground ,lazycat-red :background ,lazycat-base0 :weight ultra-bold))))
   `(paren-face-mismatch ((t (:foreground ,lazycat-base0 :background ,lazycat-red :weight ultra-bold))))
   `(paren-face-no-match ((t (:inherit paren-face-mismatch :weight ultra-bold))))

   ;; parenface
   `(paren-face ((t (:foreground ,lazycat-comments))))

   ;; parinfer
   `(parinfer-pretty-parens:dim-paren-face ((t (:foreground ,lazycat-base5))))
   `(parinfer-smart-tab:indicator-face     ((t (:foreground ,lazycat-base5))))

   ;; popup
   `(popup-face           ((t (:inherit tooltip))))
   `(popup-tip-face       ((t (:inherit popup-face :foreground ,lazycat-violet :background ,lazycat-base0))))
   `(popup-selection-face ((t (:background ,lazycat-selection))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,lazycat-blue))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,lazycat-magenta))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,lazycat-green))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,lazycat-orange))))
   `(rainbow-delimiters-depth-5-face    ((t (:foreground ,lazycat-violet))))
   `(rainbow-delimiters-depth-6-face    ((t (:foreground ,lazycat-yellow))))
   `(rainbow-delimiters-depth-7-face    ((t (:foreground ,lazycat-teal))))
   `(rainbow-delimiters-unmatched-face  ((t (:foreground ,lazycat-red :weight bold :inverse-video t))))
   `(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face))))

   ;; re-builder
   `(reb-match-0 ((t (:foreground ,lazycat-orange :inverse-video t))))
   `(reb-match-1 ((t (:foreground ,lazycat-magenta :inverse-video t))))
   `(reb-match-2 ((t (:foreground ,lazycat-green :inverse-video t))))
   `(reb-match-3 ((t (:foreground ,lazycat-yellow :inverse-video t))))

   ;; show-paren
   `(show-paren-match    ((t (:foreground ,lazycat-red :underline t :weight normal))))
   `(show-paren-mismatch ((t (:foreground ,lazycat-base0 :background ,lazycat-red :weight normal))))

   ;; smartparens
   `(sp-pair-overlay-face       ((t (:background ,lazycat-region))))
   `(sp-show-pair-match-face    ((t (:foreground ,lazycat-red :background ,lazycat-base0 :weight ultra-bold))))
   `(sp-show-pair-mismatch-face ((t (:foreground ,lazycat-base0 :background ,lazycat-red :weight ultra-bold))))

   ;; tabbar
   `(tabbar-default             ((t (:foreground ,lazycat-background :background ,lazycat-background :height 1.0))))
   `(tabbar-highlight           ((t (:foreground ,lazycat-foreground :background ,lazycat-selection :distant-foreground ,lazycat-background))))
   `(tabbar-button              ((t (:foreground ,lazycat-foreground :background ,lazycat-background))))
   `(tabbar-button-highlight    ((t (:inherit tabbar-button :inverse-video t))))
   `(tabbar-modified            ((t (:inherit tabbar-default :foreground ,lazycat-red :weight bold))))
   `(tabbar-unselected          ((t (:inherit tabbar-default :foreground ,lazycat-base5))))
   `(tabbar-unselected-modified ((t (:inherit tabbar-modified))))
   `(tabbar-selected            ((t (:inherit tabbar-default :weight bold :foreground ,lazycat-foreground :background ,lazycat-background-alt))))
   `(tabbar-selected-modified   ((t (:inherit tabbar-selected :foreground ,lazycat-green))))

   ;; treemacs
   `(treemacs-root-face          ((t (:inherit font-lock-string-face :weight bold :height 1.2))))
   `(treemacs-file-face          ((t (:foreground ,lazycat-foreground))))
   `(treemacs-directory-face     ((t (:foreground ,lazycat-foreground))))
   `(treemacs-tags-face          ((t (:foreground ,lazycat-highlight))))
   `(treemacs-git-modified-face  ((t (:foreground ,lazycat-violet))))
   `(treemacs-git-added-face     ((t (:foreground ,lazycat-green))))
   `(treemacs-git-conflict-face  ((t (:foreground ,lazycat-red))))
   `(treemacs-git-untracked-face ((t (:inherit font-lock-doc-face))))

   ;; vimish-fold
   `(vimish-fold-overlay ((t (:inherit font-lock-comment-face :background ,lazycat-base0 :weight light))))
   `(vimish-fold-fringe  ((t (:foreground ,lazycat-magenta))))
   `(vhl/default-face    ((t (:background ,lazycat-grey))))

   ;; wgrep
   `(wgrep-face        ((t (:weight bold :foreground ,lazycat-green :background ,lazycat-base5))))
   `(wgrep-delete-face ((t (:foreground ,lazycat-base3 :background ,lazycat-red))))
   `(wgrep-done-face   ((t (:foreground ,lazycat-blue))))
   `(wgrep-file-face   ((t (:foreground ,lazycat-comments))))
   `(wgrep-reject-face ((t (:foreground ,lazycat-red :weight bold))))

   ;; which-func
   `(which-func                           ((t (:foreground ,lazycat-blue))))
   `(which-key-key-face                   ((t (:foreground ,lazycat-green))))
   `(which-key-group-description-face     ((t (:foreground ,lazycat-violet))))
   `(which-key-command-description-face   ((t (:foreground ,lazycat-blue))))
   `(which-key-local-map-description-face ((t (:foreground ,lazycat-magenta))))

   ;; whitespace
   `(whitespace-empty       ((t (:background ,lazycat-base3))))
   `(whitespace-space       ((t (:foreground ,lazycat-base4))))
   `(whitespace-tab         ((t (:foreground ,lazycat-base4 :background ,(unless (default-value 'indent-tabs-mode) lazycat-base3)))))
   `(whitespace-newline     ((t (:foreground ,lazycat-base4))))
   `(whitespace-indentation ((t (:foreground ,lazycat-red :background ,lazycat-yellow))))
   `(whitespace-trailing    ((t (:inherit trailing-whitespace))))
   `(whitespace-line        ((t (:background ,lazycat-base0 :foreground ,lazycat-red :weight bold))))

   ;; yasnippet
   `(yas-field-highlight-face ((t (:inherit match))))


   ;; --- major-mode faces -------------------
   ;; elixir-mode
   `(elixir-atom-face      ((t (:foreground ,lazycat-cyan))))
   `(elixir-attribute-face ((t (:foreground ,lazycat-violet))))

   ;; enh-rube-mode
   `(enh-ruby-op-face                ((t (:foreground ,lazycat-operator))))
   `(enh-ruby-string-delimiter-face  ((t (:inherit font-lock-string-face))))
   `(enh-ruby-heredoc-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-regexp-face            ((t (:foreground ,lazycat-constant))))
   `(enh-ruby-regexp-delimiter-face  ((t (:inherit enh-ruby-regexp-face))))
   `(erm-syn-errline                 ((t (:underline (:style wave :color ,lazycat-error)))))
   `(erm-syn-warnline                ((t (:underline (:style wave :color ,lazycat-warning)))))

   ;; js2-mode
   `(js2-function-param    ((t (:foreground ,lazycat-variable))))
   `(js2-function-call     ((t (:foreground ,lazycat-function))))
   `(js2-object-property   ((t (:foreground ,lazycat-violet))))
   `(js2-jsdoc-tag         ((t (:foreground ,lazycat-doc-comments))))
   `(js2-external-variable ((t (:foreground ,lazycat-operator))))

   ;; makefile-*-mode
   `(makefile-targets ((t (:foreground ,lazycat-blue))))

   ;; man-mode
   `(Man-overstrike ((t (:inherit bold :foreground ,lazycat-operator))))
   `(Man-underline  ((t (:inherit underline :foreground ,lazycat-keyword))))

   ;; markdown-mode
   `(markdown-header-face             ((t (:inherit bold :foreground ,lazycat-red))))
   `(markdown-header-delimiter-face   ((t (:inherit markdown-header-face))))
   `(markdown-metadata-key-face       ((t (:foreground ,lazycat-red))))
   `(markdown-list-face               ((t (:foreground ,lazycat-red))))
   `(markdown-link-face               ((t (:foreground ,lazycat-highlight))))
   `(markdown-url-face                ((t (:foreground ,lazycat-magenta :weight normal))))
   `(markdown-italic-face             ((t (:inherit italic :foreground ,lazycat-violet))))
   `(markdown-bold-face               ((t (:inherit bold :foreground ,lazycat-orange))))
   `(markdown-markup-face             ((t (:foreground ,lazycat-base5))))
   `(markdown-blockquote-face         ((t (:inherit italic :foreground ,lazycat-doc-comments))))
   `(markdown-pre-face                ((t (:foreground ,lazycat-string))))
   `(markdown-code-face               ((t (:background ,(lazycat-lighten lazycat-base3 0.05) :extend t))))
   `(markdown-reference-face          ((t (:foreground ,lazycat-doc-comments))))
   `(markdown-inline-code-face        ((t (:inherit (markdown-code-face markdown-pre-face) :extend nil))))
   `(markdown-html-attr-name-face     ((t (:inherit font-lock-variable-name-face))))
   `(markdown-html-attr-value-face    ((t (:inherit font-lock-string-face))))
   `(markdown-html-entity-face        ((t (:inherit font-lock-variable-name-face))))
   `(markdown-html-tag-delimiter-face ((t (:inherit markdown-markup-face))))
   `(markdown-html-tag-name-face      ((t (:inherit font-lock-keyword-face))))

   ;; css-mode / scss-mode
   `(css-proprietary-property ((t (:foreground ,lazycat-orange))))
   `(css-property             ((t (:foreground ,lazycat-green))))
   `(css-selector             ((t (:foreground ,lazycat-blue))))

   ;; outline
   `(outline-1 ((t (:foreground ,lazycat-blue :weight bold :extend t))))
   `(outline-2 ((t (:foreground ,lazycat-magenta :weight bold :extend t))))
   `(outline-3 ((t (:foreground ,lazycat-violet :weight bold :extend t))))
   `(outline-4 ((t (:foreground ,(lazycat-lighten lazycat-blue 0.25) :weight bold :extend t))))
   `(outline-5 ((t (:foreground ,(lazycat-lighten lazycat-magenta 0.25) :weight bold :extend t))))
   `(outline-6 ((t (:foreground ,(lazycat-lighten lazycat-blue 0.5) :weight bold :extend t))))
   `(outline-7 ((t (:foreground ,(lazycat-lighten lazycat-magenta 0.5) :weight bold :extend t))))
   `(outline-8 ((t (:foreground ,(lazycat-lighten lazycat-blue 0.8) :weight bold :extend t))))

   ;; org-mode
   `(org-archived                 ((t (:foreground ,lazycat-doc-comments))))
   `(org-block                    ((t (:background ,lazycat-base3 :extend t))))
   `(org-block-background         ((t (:background ,lazycat-base3 :extend t))))
   `(org-block-begin-line         ((t (:foreground ,lazycat-comments :background ,lazycat-base3 :extend t))))
   `(org-block-end-line           ((t (:inherit org-block-begin-line))))
   `(org-checkbox                 ((t (:inherit org-todo))))
   `(org-checkbox-statistics-done ((t (:inherit org-done))))
   `(org-checkbox-statistics-todo ((t (:inherit org-todo))))
   `(org-code                     ((t (:foreground ,lazycat-orange))))
   `(org-date                     ((t (:foreground ,lazycat-yellow))))
   `(org-default                  ((t (:inherit variable-pitch))))
   `(org-document-info            ((t (:foreground ,lazycat-builtin))))
   `(org-document-title           ((t (:foreground ,lazycat-builtin :weight bold))))
   `(org-done                     ((t (:inherit org-headline-done))))
   `(org-ellipsis                 ((t (:underline nil :background nil :foreground ,lazycat-grey))))
   `(org-footnote                 ((t (:foreground ,lazycat-orange))))
   `(org-formula                  ((t (:foreground ,lazycat-cyan))))
   `(org-headline-done            ((t (:foreground ,lazycat-base5))))
   `(org-hide                     ((t (:foreground ,lazycat-hidden))))

   `(org-list-dt           ((t (:foreground ,lazycat-highlight))))
   `(org-meta-line         ((t (:foreground ,lazycat-doc-comments))))
   `(org-priority          ((t (:foreground ,lazycat-red))))
   `(org-property-value    ((t (:foreground ,lazycat-doc-comments))))
   `(org-quote             ((t (:background ,lazycat-base3 :slant italic :extend t))))
   `(org-special-keyword   ((t (:foreground ,lazycat-doc-comments))))
   `(org-table             ((t (:foreground ,lazycat-violet))))
   `(org-tag               ((t (:foreground ,lazycat-doc-comments :weight normal))))
   `(org-ref-cite-face     ((t (:foreground ,lazycat-yellow :weight light :underline t))))
   `(org-latex-and-related ((t (:foreground ,lazycat-base8 :weight bold))))
   `(org-todo              ((t (:foreground ,lazycat-green))))
   `(org-verbatim          ((t (:foreground ,lazycat-green))))
   `(org-warning           ((t (:foreground ,lazycat-warning))))

   ;; org-agenda
   `(org-agenda-done               ((t (:inherit org-done))))
   `(org-agenda-dimmed-todo-face   ((t (:foreground ,lazycat-comments))))
   `(org-agenda-date               ((t (:foreground ,lazycat-violet :weight ultra-bold))))
   `(org-agenda-date-today         ((t (:foreground ,(lazycat-lighten lazycat-violet 0.4) :weight ultra-bold))))
   `(org-agenda-date-weekend       ((t (:foreground ,(lazycat-darken lazycat-violet 0.4) :weight ultra-bold))))
   `(org-agenda-structure          ((t (:foreground ,lazycat-foreground :weight ultra-bold))))
   `(org-agenda-clocking           ((t (:background ,(lazycat-blend lazycat-blue lazycat-background 0.2)))))
   `(org-upcoming-deadline         ((t (:foreground ,(lazycat-blend lazycat-foreground lazycat-background 0.8)))))
   `(org-upcoming-distant-deadline ((t (:foreground ,(lazycat-blend lazycat-foreground lazycat-background 0.5)))))
   `(org-scheduled                 ((t (:foreground ,lazycat-foreground))))
   `(org-scheduled-today           ((t (:foreground ,lazycat-base7))))
   `(org-scheduled-previously      ((t (:foreground ,lazycat-base8))))
   `(org-time-grid                 ((t (:foreground ,lazycat-comments))))
   `(org-sexp-date                 ((t (:foreground ,lazycat-foreground))))

   ;; typescript-mode
   `(typescript-jsdoc-tag   ((t (:foreground ,lazycat-doc-comments))))
   `(typescript-jsdoc-type  ((t (:foreground ,(lazycat-darken lazycat-doc-comments 0.15)))))
   `(typescript-jsdoc-value ((t (:foreground ,(lazycat-lighten lazycat-doc-comments 0.15)))))

   ;; sh-mode
   `(sh-heredoc     ((t (:inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))

   ;; web-mode
   `(web-mode-doctype-face          ((t (:foreground ,lazycat-comments))))
   `(web-mode-html-tag-face         ((t (:foreground ,lazycat-method))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,lazycat-method))))
   `(web-mode-html-attr-name-face   ((t (:foreground ,lazycat-type))))
   `(web-mode-html-entity-face      ((t (:foreground ,lazycat-cyan :inherit italic))))
   `(web-mode-block-control-face    ((t (:foreground ,lazycat-orange))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,lazycat-operator))))

   ;; woman
   `(woman-bold   ((t (:inherit Man-overstrike))))
   `(woman-italic ((t (:inherit Man-underline)))))

  (custom-theme-set-variables
   'lazycat
   `(ansi-color-names-vector [,lazycat-background ,lazycat-red ,lazycat-green ,lazycat-yellow ,lazycat-blue ,lazycat-magenta ,lazycat-cyan ,lazycat-foreground])
   `(rustic-ansi-faces [,lazycat-background ,lazycat-red ,lazycat-green ,lazycat-yellow ,lazycat-blue ,lazycat-magenta ,lazycat-cyan ,lazycat-foreground])
   `(vc-annotate-color-map
     (list (cons 20  ,lazycat-green)
           (cons 40  ,(lazycat-blend lazycat-yellow lazycat-green (/ 1.0 3)))
           (cons 60  ,(lazycat-blend lazycat-yellow lazycat-green (/ 2.0 3)))
           (cons 80  ,lazycat-yellow)
           (cons 100 ,(lazycat-blend lazycat-orange lazycat-yellow (/ 1.0 3)))
           (cons 120 ,(lazycat-blend lazycat-orange lazycat-yellow (/ 2.0 3)))
           (cons 140 ,lazycat-orange)
           (cons 160 ,(lazycat-blend lazycat-magenta lazycat-orange (/ 1.0 3)))
           (cons 180 ,(lazycat-blend lazycat-magenta lazycat-orange (/ 2.0 3)))
           (cons 200 ,lazycat-magenta)
           (cons 220 ,(lazycat-blend lazycat-red lazycat-magenta (/ 1.0 3)))
           (cons 240 ,(lazycat-blend lazycat-red lazycat-magenta (/ 2.0 3)))
           (cons 260 ,lazycat-red)
           (cons 280 ,(lazycat-blend lazycat-grey lazycat-red (/ 1.0 4)))
           (cons 300 ,(lazycat-blend lazycat-grey lazycat-red (/ 2.0 4)))
           (cons 320 ,(lazycat-blend lazycat-grey lazycat-red (/ 3.0 4)))
           (cons 340 ,lazycat-base5)
           (cons 360 ,lazycat-base5)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background ,lazycat-background)))


;; (set-face-bold 'bold nil)
;; (set-face-italic 'italic nil)

(provide-theme 'lazycat)
