(require 'cl-lib)

(defgroup lazycat-themes nil
  "Options for lazycat-themes."
  :group 'faces)

(defcustom lazycat-themes-enable-bold nil
  "If nil, bold will be disabled across all faces."
  :group 'lazycat-themes
  :type 'boolean)

(defcustom lazycat-themes-enable-italic nil
  "If nil, italics will be disabled across all faces."
  :group 'lazycat-themes
  :type 'boolean)

(defcustom lazycat-dark-comment-bg nil
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'lazycat-themes
  :type 'boolean)

(defvar lazycat-themes--colors nil)
(defvar lazycat--min-colors '(257 256 16))
(defvar lazycat--quoted-p nil)
(defvar lazycat-themes--faces nil)

(defun lazycat-themes--colors-p (item)
  (declare (pure t) (side-effect-free t))
  (when item
    (cond ((listp item)
           (let ((car (car item)))
             (cond ((memq car '(quote lazycat-color)) nil)

                   ((memq car '(backquote \`))
                    (let ((lazycat--quoted-p t))
                      (lazycat-themes--colors-p (cdr item))))

                   ((eq car '\,)
                    (let (lazycat--quoted-p)
                      (lazycat-themes--colors-p (cdr item))))

                   ((or (lazycat-themes--colors-p car)
                        (lazycat-themes--colors-p (cdr-safe item)))))))

          ((and (symbolp item)
                (not (keywordp item))
                (not lazycat--quoted-p)
                (not (equal (substring (symbol-name item) 0 1) "-"))
                (assq item lazycat-themes--colors))))))

(defun lazycat-themes--colorize (item type)
  (declare (pure t) (side-effect-free t))
  (when item
    (let ((lazycat--quoted-p lazycat--quoted-p))
      (cond ((listp item)
             (cond ((memq (car item) '(quote lazycat-color))
                    item)
                   ((eq (car item) 'lazycat-ref)
                    (lazycat-themes--colorize
                     (apply #'lazycat-ref (cdr item)) type))
                   ((let* ((item (append item nil))
                           (car (car item))
                           (lazycat--quoted-p
                            (cond ((memq car '(backquote \`)) t)
                                  ((eq car '\,) nil)
                                  (t lazycat--quoted-p))))
                      (cons car
                            (cl-loop
                             for i in (cdr item)
                             collect (lazycat-themes--colorize i type)))))))

            ((and (symbolp item)
                  (not (keywordp item))
                  (not lazycat--quoted-p)
                  (not (equal (substring (symbol-name item) 0 1) "-"))
                  (assq item lazycat-themes--colors))
             `(lazycat-color ',item ',type))

            (item)))))

(defun lazycat-themes--build-face (face)
  (declare (pure t) (side-effect-free t))
  `(list
    ',(car face)
    ,(let ((face-body (cdr face)))
       (cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                ;; (if (lazycat-themes--colors-p real-attrs)
                ;;     (dolist (cl lazycat--min-colors `(list ,@(nreverse defs)))
                ;;       (push `(list '((class color) (min-colors ,cl))
                ;;                    (list ,@(lazycat-themes--colorize real-attrs cl)))
                ;;             defs))
                `(list (list 't (list ,@real-attrs)))
                ;; )
                ))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))))))

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
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (lazycat-blend (lazycat-color color1) (lazycat-color color2) alpha))

          ((or (listp color1) (listp color2))
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
  (cond ((and color (symbolp color))
         (lazycat-darken (lazycat-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (lazycat-darken c alpha)))

        ((lazycat-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun lazycat-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1)."
  (cond ((and color (symbolp color))
         (lazycat-lighten (lazycat-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (lazycat-lighten c alpha)))

        ((lazycat-blend color "#FFFFFF" (- 1 alpha)))))

;;;###autoload
(defun lazycat-color (name &optional type)
  "Retrieve a specific color named NAME (a symbol) from the current theme."
  (let ((colors (if (listp name)
                    name
                  (cdr-safe (assq name lazycat-themes--colors)))))
    (and colors
         (cond ((listp colors)
                (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                  (if (> i (1- (length colors)))
                      (car (last colors))
                    (nth i colors))))
               (t colors)))))

;;;###autoload
(defun lazycat-ref (face prop &optional class)
  "TODO"
  (let ((spec (or (cdr (assq face lazycat-themes--faces))
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

(defun lazycat-themes-prepare-facelist ()
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (declare (pure t) (side-effect-free t))
  (setq lazycat-themes--faces lazycat-themes-base-faces)
  (mapcar #'lazycat-themes--build-face lazycat-themes--faces))

(defmacro def-lazycat-theme (name docstring defs)
  "Define a LAZYCAT theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (let ((lazycat-themes--colors defs))
    `(let* ((bold   lazycat-themes-enable-bold)
            (italic lazycat-themes-enable-italic)
            ,@defs)
       (setq lazycat-themes--colors
             (list ,@(cl-loop for (var val) in defs
                              collect `(cons ',var ,val))))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces
        ',name ,@(lazycat-themes-prepare-facelist))
       (unless bold (set-face-bold 'bold nil))
       (unless italic (set-face-italic 'italic nil))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))


(defvar lazycat-themes-base-faces
  '(;; --- base faces -------------------------
    (bold        :weight 'bold :foreground (unless bold base8))
    (italic      :slant  'italic)
    (bold-italic :inherit '(bold italic))

    (default :background bg :foreground fg)
    (fringe :inherit 'default :foreground base4)
    (region               :background region     :foreground region-fg)
    (highlight            :background highlight  :foreground base0 :distant-foreground base8)
    (cursor               :background highlight)
    (shadow               :foreground base5)
    (minibuffer-prompt    :foreground highlight)
    (tooltip              :background base3 :foreground fg)
    (secondary-selection  :background grey :extend t)
    (lazy-highlight       :background dark-blue  :foreground base8 :distant-foreground base0 :weight 'bold)
    (match                :foreground green      :background base0 :weight 'bold)
    (trailing-whitespace  :background red)
    (nobreak-space        :inherit 'default :underline nil)
    (vertical-border      :background vertical-bar :foreground vertical-bar)
    (link                 :foreground highlight :underline t :weight 'bold)

    (error   :foreground error)
    (warning :foreground warning)
    (success :foreground success)

    (font-lock-builtin-face              :foreground builtin)
    (font-lock-comment-face
     :foreground comments
     :background (if lazycat-dark-comment-bg (lazycat-lighten bg 0.05)))
    (font-lock-comment-delimiter-face    :inherit 'font-lock-comment-face)
    (font-lock-doc-face                  :inherit 'font-lock-comment-face :foreground doc-comments)
    (font-lock-constant-face             :foreground constants)
    (font-lock-function-name-face        :foreground functions)
    (font-lock-keyword-face              :foreground keywords)
    (font-lock-string-face               :foreground strings)
    (font-lock-type-face                 :foreground type)
    (font-lock-variable-name-face        :foreground variables)
    (font-lock-warning-face              :inherit 'warning)
    (font-lock-negation-char-face        :inherit 'bold :foreground operators)
    (font-lock-preprocessor-face         :inherit 'bold :foreground operators)
    (font-lock-preprocessor-char-face    :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-backslash :inherit 'bold :foreground operators)
    (font-lock-regexp-grouping-construct :inherit 'bold :foreground operators)

    ;; mode-line
    (mode-line           :background bg     :foreground bg-alt)
    (mode-line-inactive  :background bg     :foreground fg)
    (mode-line-emphasis  :foreground highlight :distant-foreground bg   :height 0.1)
    (mode-line-highlight :inherit 'highlight :distant-foreground bg     :height 0.1)
    (mode-line-buffer-id :weight 'bold  :height 0.1)

    ;; header-line.
    (header-line :inherit 'default :height (face-attribute 'default :height))

    ;; 1. Line number faces must explicitly disable its text style attributes
    ;;    because nearby faces may "bleed" into the line numbers otherwise.
    ;; 2. All other line number plugin faces should &inherit from these.
    (line-number
     :inherit 'default
     :foreground base5 :distant-foreground nil
     :weight 'normal :italic nil :underline nil :strike-through nil)
    (line-number-current-line
     :inherit '(hl-line default)
     :foreground fg :distant-foreground nil
     :weight 'normal :italic nil :underline nil :strike-through nil)


    ;; --- built-in plugin faces --------------
    ;; centaur-tabs
    (centaur-tabs-default    :background bg-alt :foreground bg-alt)
    (centaur-tabs-selected   :background bg :foreground fg)
    (centaur-tabs-unselected :background bg-alt :foreground fg-alt)
    (centaur-tabs-selected-modified   :background bg :foreground teal)
    (centaur-tabs-unselected-modified :background bg-alt :foreground teal)
    (centaur-tabs-active-bar-face
     :background (if (bound-and-true-p -modeline-bright) modeline-bg highlight))
    (centaur-tabs-modified-marker-selected
     :inherit 'centaur-tabs-selected
     :foreground (if (bound-and-true-p -modeline-bright) modeline-bg highlight))
    (centaur-tabs-modified-marker-unselected
     :inherit 'centaur-tabs-unselected
     :foreground (if (bound-and-true-p -modeline-bright) modeline-bg highlight))

    ;; cperl
    (cperl-array-face          :weight 'bold :inherit 'font-lock-variable-name-face)
    (cperl-hash-face           :weight 'bold :slant 'italic :inherit 'font-lock-variable-name-face)
    (cperl-nonoverridable-face :inherit 'font-lock-builtin-face)

    ;; compilation
    (compilation-column-number  :inherit 'font-lock-comment-face)
    (compilation-line-number    :foreground highlight)
    (compilation-error   :inherit 'error   :weight 'bold)
    (compilation-warning :inherit 'warning :slant 'italic)
    (compilation-info    :inherit 'success)
    (compilation-mode-line-exit :inherit 'compilation-info)
    (compilation-mode-line-fail :inherit 'compilation-error)

    ;; custom
    (custom-button                  :foreground blue   :background bg     :box '(:line-width 1 :style none))
    (custom-button-unraised         :foreground violet :background bg     :box '(:line-width 1 :style none))
    (custom-button-pressed-unraised :foreground bg     :background violet :box '(:line-width 1 :style none))
    (custom-button-pressed          :foreground bg     :background blue   :box '(:line-width 1 :style none))
    (custom-button-mouse            :foreground bg     :background blue   :box '(:line-width 1 :style none))

    (custom-variable-button   :foreground green :underline t)
    (custom-saved             :foreground green :background (lazycat-blend green bg 0.2) :bold bold)
    (custom-comment           :foreground fg :background region)
    (custom-comment-tag       :foreground grey)
    (custom-modified          :foreground blue :background (lazycat-blend blue bg 0.2))
    (custom-variable-tag      :foreground magenta)
    (custom-visibility        :foreground blue :underline nil)
    (custom-group-subtitle    :foreground red)
    (custom-group-tag         :foreground violet)
    (custom-group-tag-1       :foreground blue)
    (custom-set               :foreground yellow :background bg)
    (custom-themed            :foreground yellow :background bg)
    (custom-invalid           :foreground red :background (lazycat-blend red bg 0.2))
    (custom-variable-obsolete :foreground grey :background bg)
    (custom-state             :foreground green :background (lazycat-blend green bg 0.2))
    (custom-changed           :foreground blue :background bg)

    ;; dired
    (dired-directory  :foreground builtin)
    (dired-ignored    :foreground comments)
    (dired-flagged    :foreground red)
    (dired-header     :foreground variables :weight 'bold)
    (dired-mark       :foreground orange :weight 'bold)
    (dired-marked     :foreground magenta :weight 'bold :inverse-video t)
    (dired-perm-write :foreground fg :underline t)
    (dired-symlink    :foreground cyan :weight 'bold)
    (dired-warning    :foreground warning)

    ;; ediff
    (ediff-fine-diff-A    :background (lazycat-blend selection bg 0.7) :weight 'bold :extend t)
    (ediff-fine-diff-B    :inherit 'ediff-fine-diff-A)
    (ediff-fine-diff-C    :inherit 'ediff-fine-diff-A)
    (ediff-current-diff-A :background (lazycat-blend selection bg 0.3) :extend t)
    (ediff-current-diff-B :inherit 'ediff-current-diff-A)
    (ediff-current-diff-C :inherit 'ediff-current-diff-A)
    (ediff-even-diff-A    :inherit 'hl-line)
    (ediff-even-diff-B    :inherit 'ediff-even-diff-A)
    (ediff-even-diff-C    :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-A     :inherit 'ediff-even-diff-A)
    (ediff-odd-diff-B     :inherit 'ediff-odd-diff-A)
    (ediff-odd-diff-C     :inherit 'ediff-odd-diff-A)

    ;; eshell
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

    ;; flx-ido
    (flx-highlight-face :weight 'bold :foreground yellow :underline nil)

    ;; hi-lock
    (hi-yellow   :background yellow)
    (hi-pink     :background magenta)
    (hi-red-b    :foreground red :weight 'bold)
    (hi-green    :background green)
    (hi-green-b  :foreground green :weight 'bold)
    (hi-blue     :background blue)
    (hi-blue-b   :foreground blue :weight 'bold)
    ;; (hi-black-b  :weight 'bold)
    ;; (hi-black-hb :inherit 'variable-pitch :weight 'bold :height 1.67)

    ;; hl-line
    (hl-line :background bg-alt :extend t)

    ;; ido
    (ido-first-match :foreground orange)
    (ido-indicator   :foreground red :background bg)
    (ido-only-match  :foreground green)
    (ido-subdir      :foreground violet)
    (ido-virtual     :foreground comments)

    ;; isearch
    (isearch :inherit 'lazy-highlight :weight 'bold)
    (isearch-fail :background error :foreground base0 :weight 'bold)

    ;; linum totally inherit line-number
    (linum :inherit 'default
           :foreground base5 :distant-foreground nil
           :weight 'normal :italic nil :underline nil :strike-through nil)

    ;; message
    (message-header-name       :foreground green)
    (message-header-subject    :foreground highlight :weight 'bold)
    (message-header-to         :foreground highlight :weight 'bold)
    (message-header-cc         :inherit 'message-header-to :foreground (lazycat-darken highlight 0.15))
    (message-header-other      :foreground violet)
    (message-header-newsgroups :foreground yellow)
    (message-header-xheader    :foreground doc-comments)
    (message-separator         :foreground comments)
    (message-mml               :foreground comments :slant 'italic)
    (message-cited-text        :foreground magenta)

    ;; term
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

    ;; vterm
    (vterm               :foreground fg)
    (vterm-color-black   :background (lazycat-lighten base0 0.25)   :foreground base0)
    (vterm-color-red     :background (lazycat-lighten red 0.25)     :foreground red)
    (vterm-color-green   :background (lazycat-lighten green 0.25)   :foreground green)
    (vterm-color-yellow  :background (lazycat-lighten yellow 0.25)  :foreground yellow)
    (vterm-color-blue    :background (lazycat-lighten blue 0.25)    :foreground blue)
    (vterm-color-magenta :background (lazycat-lighten magenta 0.25) :foreground magenta)
    (vterm-color-cyan    :background (lazycat-lighten cyan 0.25)    :foreground cyan)
    (vterm-color-white   :background (lazycat-lighten base8 0.25)   :foreground base8)

    ;; widget
    (widget-button-pressed :foreground red)
    (widget-documentation  :foreground green)

    ;; window-divider
    (window-divider :inherit 'vertical-border)
    (window-divider-first-pixel :inherit 'window-divider)
    (window-divider-last-pixel  :inherit 'window-divider)


    ;; --- plugin faces -----------------------
    ;; all-the-icons
    (all-the-icons-red      :foreground red)
    (all-the-icons-lred     :foreground (lazycat-lighten red 0.3))
    (all-the-icons-dred     :foreground (lazycat-darken red 0.3))
    (all-the-icons-green    :foreground green)
    (all-the-icons-lgreen   :foreground (lazycat-lighten green 0.3))
    (all-the-icons-dgreen   :foreground (lazycat-darken green 0.3))
    (all-the-icons-yellow   :foreground yellow)
    (all-the-icons-lyellow  :foreground (lazycat-lighten yellow 0.3))
    (all-the-icons-dyellow  :foreground (lazycat-darken yellow 0.3))
    (all-the-icons-blue     :foreground blue)
    (all-the-icons-blue-alt :foreground teal)
    (all-the-icons-lblue    :foreground (lazycat-lighten blue 0.3))
    (all-the-icons-dblue    :foreground dark-blue)
    (all-the-icons-maroon   :foreground magenta)
    (all-the-icons-lmaroon  :foreground (lazycat-lighten magenta 0.3))
    (all-the-icons-dmaroon  :foreground (lazycat-darken magenta 0.3))
    (all-the-icons-purple   :foreground violet)
    (all-the-icons-lpurple  :foreground (lazycat-lighten violet 0.3))
    (all-the-icons-dpurple  :foreground (lazycat-darken violet 0.3))
    (all-the-icons-cyan     :foreground cyan)
    (all-the-icons-cyan-alt :foreground cyan)
    (all-the-icons-lcyan    :foreground (lazycat-lighten cyan 0.3))
    (all-the-icons-dcyan    :foreground dark-cyan)
    (all-the-icons-pink     :foreground (lazycat-lighten red 0.35))
    (all-the-icons-lpink    :foreground (lazycat-lighten red 0.55))
    (all-the-icons-dpink    :foreground red)
    (all-the-icons-silver   :foreground (lazycat-lighten grey 0.45))
    (all-the-icons-lsilver  :foreground (lazycat-lighten grey 0.7))
    (all-the-icons-dsilver  :foreground (lazycat-lighten grey 0.1))

    ;; all-the-icons-dired
    (all-the-icons-dired-dir-face    :foreground doc-comments)

    ;; anzu
    (anzu-replace-highlight :background base0 :foreground red   :weight 'bold :strike-through t)
    (anzu-replace-to        :background base0 :foreground green :weight 'bold)

    ;; avy
    (avy-background-face :foreground comments)
    (avy-lead-face :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (avy-lead-face-0 :inherit 'avy-lead-face :background (lazycat-lighten highlight 0.3))
    (avy-lead-face-1 :inherit 'avy-lead-face :background (lazycat-lighten highlight 0.6))
    (avy-lead-face-2 :inherit 'avy-lead-face :background (lazycat-lighten highlight 0.9))

    ;; bookmark+
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

    ;; company
    (company-tooltip            :inherit 'tooltip)
    (company-tooltip-common                           :foreground highlight :distant-foreground base0 :weight 'bold)
    (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg :weight 'bold)
    (company-tooltip-search-selection :background (lazycat-darken selection 0.25))
    (company-tooltip-selection  :background selection :weight 'bold)
    (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
    (company-tooltip-annotation                       :foreground type :distant-foreground bg)
    (company-scrollbar-bg       :inherit 'tooltip)
    (company-scrollbar-fg       :background highlight)
    (company-preview                              :foreground comments)
    (company-preview-common     :background base3 :foreground highlight)
    (company-preview-search     :inherit 'company-tooltip-search)
    (company-template-field     :inherit 'match)

    ;; company-box
    (company-box-candidate :foreground fg)

    ;; diff-hl
    (diff-hl-change :foreground vc-modified :background vc-modified)
    (diff-hl-delete :foreground vc-deleted :background vc-deleted)
    (diff-hl-insert :foreground vc-added :background vc-added)

    ;; diff-mode
    (diff-added   :inherit 'hl-line :foreground green)
    (diff-changed :foreground violet)
    (diff-context :foreground (lazycat-darken fg 0.12))
    (diff-removed :foreground red :background base3)
    (diff-header  :foreground cyan :background nil)
    (diff-file-header :foreground blue :background nil)
    (diff-hunk-header :foreground violet)
    (diff-refine-added   :inherit 'diff-added :inverse-video t)
    (diff-refine-changed :inherit 'diff-changed :inverse-video t)
    (diff-refine-removed :inherit 'diff-removed :inverse-video t)

    ;; flycheck
    (flycheck-error     :underline `(:style wave :color ,red))
    (flycheck-warning   :underline `(:style wave :color ,yellow))
    (flycheck-info      :underline `(:style wave :color ,green))

    ;; flycheck-posframe
    (flycheck-posframe-face :inherit 'default)
    (flycheck-posframe-background-face :background bg-alt)
    (flycheck-posframe-error-face   :inherit 'flycheck-posframe-face :foreground error)
    (flycheck-posframe-info-face    :inherit 'flycheck-posframe-face :foreground fg)
    (flycheck-posframe-warning-face :inherit 'flycheck-posframe-face :foreground warning)

    ;; flymake
    (flymake-error   :underline `(:style wave :color ,red))
    (flymake-note    :underline `(:style wave :color ,green))
    (flymake-warning :underline `(:style wave :color ,orange))

    ;; flyspell
    (flyspell-incorrect :underline `(:style wave :color ,error) :inherit 'unspecified)
    (flyspell-duplicate :underline `(:style wave :color ,warning) :inherit 'unspecified)

    ;; helpful
    (helpful-heading :weight 'bold :height 1.2)

    ;; highlight-quoted-mode
    (highlight-quoted-symbol :foreground type)
    (highlight-quoted-quote  :foreground operators)

    ;; highlight-numbers-mode
    (highlight-numbers-number :inherit 'bold :foreground numbers)

    ;; hlinum
    (linum-highlight-face :foreground fg :distant-foreground nil :weight 'normal)

    ;; hl-todo
    (hl-todo :foreground red :weight 'bold)

    ;; hydra
    (hydra-face-red      :foreground red     :weight 'bold)
    (hydra-face-blue     :foreground blue    :weight 'bold)
    (hydra-face-amaranth :foreground magenta :weight 'bold)
    (hydra-face-pink     :foreground violet  :weight 'bold)
    (hydra-face-teal     :foreground teal    :weight 'bold)

    ;; iedit
    (iedit-occurrence :foreground magenta :weight 'bold :inverse-video t)
    (iedit-read-only-occurrence :inherit 'region)

    ;; imenu-list
    ;; (imenu-list-entry-face)
    (imenu-list-entry-face-0 :foreground highlight)
    (imenu-list-entry-subalist-face-0 :inherit 'imenu-list-entry-face-0 :weight 'bold)
    (imenu-list-entry-face-1 :foreground green)
    (imenu-list-entry-subalist-face-1 :inherit 'imenu-list-entry-face-1 :weight 'bold)
    (imenu-list-entry-face-2 :foreground yellow)
    (imenu-list-entry-subalist-face-2 :inherit 'imenu-list-entry-face-2 :weight 'bold)

    ;; lui
    (lui-time-stamp-face :foreground violet)
    (lui-highlight-face :foreground highlight)
    (lui-button-face :foreground builtin :underline t)

    ;; multiple cursors
    (mc/cursor-face :inherit 'cursor)

    ;; nav-flash
    (nav-flash-face :background selection :foreground base8 :weight 'bold)

    ;; neotree
    (neo-root-dir-face   :foreground strings :background bg :box `(:line-width 4 :color ,bg))
    (neo-file-link-face  :foreground fg)
    (neo-dir-link-face   :foreground highlight)
    (neo-expand-btn-face :foreground highlight)
    (neo-vc-edited-face  :foreground yellow)
    (neo-vc-added-face   :foreground green)
    (neo-vc-removed-face :foreground red :strike-through t)
    (neo-vc-conflict-face :foreground magenta :weight 'bold)
    (neo-vc-ignored-face  :foreground comments)
    (lazycat-neotree-dir-face :foreground highlight)
    (lazycat-neotree-file-face :foreground base8)
    (lazycat-neotree-hidden-file-face :foreground comments)
    (lazycat-neotree-text-file-face :foreground fg)
    (lazycat-neotree-data-file-face :foreground violet)
    (lazycat-neotree-media-file-face :inherit 'lazycat-neotree-hidden-file-face)

    ;; magit
    (magit-bisect-bad        :foreground red)
    (magit-bisect-good       :foreground green)
    (magit-bisect-skip       :foreground orange)
    (magit-blame-date        :foreground red)
    (magit-blame-heading     :foreground orange :background base3 :extend t)
    (magit-branch-current    :foreground blue)
    (magit-branch-local      :foreground cyan)
    (magit-branch-remote     :foreground green)
    (magit-cherry-equivalent :foreground violet)
    (magit-cherry-unmatched  :foreground cyan)
    (magit-diff-added             :foreground (lazycat-darken green 0.2)  :background (lazycat-blend green bg 0.1) :extend t)
    (magit-diff-added-highlight   :foreground green                    :background (lazycat-blend green bg 0.2) :weight 'bold :extend t)
    (magit-diff-base              :foreground (lazycat-darken orange 0.2) :background (lazycat-blend orange bg 0.1) :extend t)
    (magit-diff-base-highlight    :foreground orange                   :background (lazycat-blend orange bg 0.2) :weight 'bold :extend t)
    (magit-diff-context           :foreground (lazycat-darken fg 0.4) :background bg :extend t)
    (magit-diff-context-highlight :foreground fg                   :background bg-alt :extend t)
    (magit-diff-file-heading           :foreground fg :weight 'bold :extend t)
    (magit-diff-file-heading-selection :foreground magenta               :background dark-blue :weight 'bold :extend t)
    (magit-diff-hunk-heading           :foreground bg                    :background (lazycat-blend violet bg 0.3) :extend t)
    (magit-diff-hunk-heading-highlight :foreground bg                    :background violet :weight 'bold :extend t)
    (magit-diff-removed                :foreground (lazycat-darken red 0.2) :background (lazycat-blend red base3 0.1) :extend t)
    (magit-diff-removed-highlight      :foreground red                   :background (lazycat-blend red base3 0.2) :weight 'bold :extend t)
    (magit-diff-lines-heading          :foreground yellow :background red :extend t :extend t)
    (magit-diffstat-added              :foreground green)
    (magit-diffstat-removed            :foreground red)
    (magit-dimmed :foreground comments)
    (magit-hash :foreground comments)
    (magit-header-line :background dark-blue :foreground base8 :weight 'bold
                       :box `(:line-width 3 :color ,dark-blue))
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
    (magit-filename :foreground violet)
    (magit-section-secondary-heading :foreground violet :weight 'bold :extend t)

    ;; mic-paren
    (paren-face-match    :foreground red   :background base0 :weight 'ultra-bold)
    (paren-face-mismatch :foreground base0 :background red   :weight 'ultra-bold)
    (paren-face-no-match :inherit 'paren-face-mismatch :weight 'ultra-bold)

    ;; objed
    (objed-mode-line :inherit 'warning :weight 'bold)
    (objed-hl        :inherit 'region :background (lazycat-blend region bg 0.5))

    ;; parenface
    (paren-face :foreground comments)

    ;; parinfer
    (parinfer-pretty-parens:dim-paren-face :foreground base5)
    (parinfer-smart-tab:indicator-face :foreground base5)

    ;; perspective
    (persp-selected-face :foreground blue :weight 'bold)

    ;; persp-mode
    (persp-face-lighter-default :foreground highlight :weight 'bold)
    (persp-face-lighter-buffer-not-in-persp :foreground doc-comments)
    (persp-face-lighter-nil-persp :foreground comments)

    ;; popup
    (popup-face :inherit 'tooltip)
    (popup-tip-face :inherit 'popup-face :foreground violet :background base0)
    (popup-selection-face :background selection)

    ;; rainbow-delimiters
    (rainbow-delimiters-depth-1-face :foreground blue)
    (rainbow-delimiters-depth-2-face :foreground magenta)
    (rainbow-delimiters-depth-3-face :foreground green)
    (rainbow-delimiters-depth-4-face :foreground orange)
    (rainbow-delimiters-depth-5-face :foreground violet)
    (rainbow-delimiters-depth-6-face :foreground yellow)
    (rainbow-delimiters-depth-7-face :foreground teal)
    (rainbow-delimiters-unmatched-face  :foreground red :weight 'bold :inverse-video t)
    (rainbow-delimiters-mismatched-face :inherit 'rainbow-delimiters-unmatched-face)

    ;; re-builder
    (reb-match-0 :foreground orange  :inverse-video t)
    (reb-match-1 :foreground magenta :inverse-video t)
    (reb-match-2 :foreground green   :inverse-video t)
    (reb-match-3 :foreground yellow  :inverse-video t)

    ;; show-paren
    (show-paren-match :foreground red :background base0 :weight 'ultra-bold)
    (show-paren-mismatch :foreground base0 :background red   :weight 'ultra-bold)

    ;; smartparens
    (sp-pair-overlay-face :background region)
    (sp-show-pair-match-face    :foreground red :background base0 :weight 'ultra-bold)
    (sp-show-pair-mismatch-face :foreground base0 :background red   :weight 'ultra-bold)

    ;; smerge-tool
    (smerge-lower :background (lazycat-blend green bg 0.2))
    (smerge-upper :background (lazycat-blend red base3 0.2))
    (smerge-base  :background (lazycat-blend blue bg 0.2))
    (smerge-markers :background comments :foreground bg :distant-foreground fg :weight 'bold)
    (smerge-refined-added   :inherit 'diff-added :inverse-video t)
    (smerge-refined-removed :inherit 'diff-removed :inverse-video t)

    ;; solaire-mode
    (solaire-default-face  :inherit 'default :background bg-alt)
    (solaire-hl-line-face  :inherit 'hl-line :background bg :extend t)
    (solaire-org-hide-face :foreground hidden)

    ;; stripe-buffer
    (stripe-highlight :background base3)

    ;; tabbar
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

    ;; tldr
    (tldr-command-itself   :foreground bg :background green :weight 'semi-bold)
    (tldr-title            :foreground yellow :bold t :height 1.4)
    (tldr-description      :foreground fg :weight 'semi-bold)
    (tldr-introduction     :foreground (lazycat-blend blue bg 0.8) :weight 'semi-bold)
    (tldr-code-block       :foreground green :background region :weight 'semi-bold)
    (tldr-command-argument :foreground fg :background region )

    ;; treemacs
    (treemacs-root-face :inherit 'font-lock-string-face :weight 'bold :height 1.2)
    (treemacs-file-face :foreground fg)
    (treemacs-directory-face :foreground fg)
    (treemacs-tags-face :foreground highlight)
    (treemacs-git-modified-face :foreground violet)
    (treemacs-git-added-face :foreground green)
    (treemacs-git-conflict-face :foreground red)
    (treemacs-git-untracked-face :inherit 'font-lock-doc-face)

    ;; vimish-fold
    (vimish-fold-overlay :inherit 'font-lock-comment-face :background base0 :weight 'light)
    (vimish-fold-fringe  :foreground magenta)

    ;; volatile-highlights
    (vhl/default-face :background grey)

    ;; wgrep
    (wgrep-face :weight 'bold :foreground green :background base5)
    (wgrep-delete-face :foreground base3 :background red)
    (wgrep-done-face   :foreground blue)
    (wgrep-file-face   :foreground comments)
    (wgrep-reject-face :foreground red :weight 'bold)

    ;; which-func
    (which-func :foreground blue)

    ;; which-key
    (which-key-key-face                   :foreground green)
    (which-key-group-description-face     :foreground violet)
    (which-key-command-description-face   :foreground blue)
    (which-key-local-map-description-face :foreground magenta)

    ;; whitespace
    (whitespace-empty    :background base3)
    (whitespace-space    :foreground base4)
    (whitespace-tab      :foreground base4 :background (unless (default-value 'indent-tabs-mode) base3))
    (whitespace-newline  :foreground base4)
    (whitespace-indentation :foreground red :background yellow)
    (whitespace-trailing :inherit 'trailing-whitespace)
    (whitespace-line     :background base0 :foreground red :weight 'bold)

    ;; workgroups2
    (wg-current-workgroup-face :foreground base0 :background highlight)
    (wg-other-workgroup-face   :foreground base5)
    (wg-divider-face           :foreground grey)
    (wg-brace-face             :foreground highlight)

    ;; yasnippet
    (yas-field-highlight-face :inherit 'match)


    ;; --- major-mode faces -------------------
    ;; elixir-mode
    (elixir-atom-face :foreground cyan)
    (elixir-attribute-face :foreground violet)

    ;; enh-ruby-mode
    (enh-ruby-op-face :foreground operators)
    (enh-ruby-string-delimiter-face  :inherit 'font-lock-string-face)
    (enh-ruby-heredoc-delimiter-face :inherit 'font-lock-string-face)
    (enh-ruby-regexp-face :foreground constants)
    (enh-ruby-regexp-delimiter-face  :inherit 'enh-ruby-regexp-face)
    (erm-syn-errline  :underline `(:style wave :color ,error))
    (erm-syn-warnline :underline `(:style wave :color ,warning))

    ;; js2-mode
    (js2-function-param    :foreground variables)
    (js2-function-call     :foreground functions)
    (js2-object-property   :foreground violet)
    (js2-jsdoc-tag         :foreground doc-comments)
    (js2-external-variable :foreground operators)

    ;; ledger-mode
    (ledger-font-posting-date-face :foreground blue)
    (ledger-font-posting-amount-face :foreground yellow)
    (ledger-font-posting-account-face :foreground base8)
    (ledger-font-payee-cleared-face :foreground violet :weight 'bold)
    (ledger-font-payee-uncleared-face :foreground base5 :weight 'bold)
    (ledger-font-xact-highlight-face :background base0)

    ;; makefile-*-mode
    (makefile-targets :foreground blue)

    ;; man-mode
    (Man-overstrike :inherit 'bold :foreground operators)
    (Man-underline :inherit 'underline :foreground keywords)

    ;; markdown-mode
    (markdown-header-face           :inherit 'bold :foreground red)
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
    (markdown-code-face             :background (lazycat-lighten base3 0.05) :extend t)
    (markdown-reference-face        :foreground doc-comments)
    (markdown-inline-code-face      :inherit '(markdown-code-face markdown-pre-face) :extend nil)
    (markdown-html-attr-name-face     :inherit 'font-lock-variable-name-face)
    (markdown-html-attr-value-face    :inherit 'font-lock-string-face)
    (markdown-html-entity-face        :inherit 'font-lock-variable-name-face)
    (markdown-html-tag-delimiter-face :inherit 'markdown-markup-face)
    (markdown-html-tag-name-face      :inherit 'font-lock-keyword-face)

    ;; css-mode / scss-mode
    (css-proprietary-property :foreground orange)
    (css-property             :foreground green)
    (css-selector             :foreground blue)

    ;; outline
    (outline-1 :foreground blue                        :weight 'bold :extend t)
    (outline-2 :foreground magenta                     :weight 'bold :extend t)
    (outline-3 :foreground violet                      :weight 'bold :extend t)
    (outline-4 :foreground (lazycat-lighten blue 0.25)    :weight 'bold :extend t)
    (outline-5 :foreground (lazycat-lighten magenta 0.25) :weight 'bold :extend t)
    (outline-6 :foreground (lazycat-lighten blue 0.5)     :weight 'bold :extend t)
    (outline-7 :foreground (lazycat-lighten magenta 0.5)  :weight 'bold :extend t)
    (outline-8 :foreground (lazycat-lighten blue 0.8)     :weight 'bold :extend t)

    ;; org-mode
    (org-archived                 :foreground doc-comments)
    (org-block                    :background base3 :extend t)
    (org-block-background         :background base3 :extend t)
    (org-block-begin-line         :foreground comments :background base3 :extend t)
    (org-block-end-line           :inherit 'org-block-begin-line)
    (org-checkbox                 :inherit 'org-todo)
    (org-checkbox-statistics-done :inherit 'org-done)
    (org-checkbox-statistics-todo :inherit 'org-todo)
    (org-code                     :foreground orange)
    (org-date                     :foreground yellow)
    (org-default                  :inherit 'variable-pitch)
    (org-document-info            :foreground builtin)
    (org-document-title           :foreground builtin :weight 'bold)
    (org-done                     :inherit 'org-headline-done :bold 'inherit)
    (org-ellipsis                 :underline nil :background nil :foreground grey)
    (org-footnote                 :foreground orange)
    (org-formula                  :foreground cyan)
    (org-headline-done            :foreground base5)
    (org-hide                     :foreground hidden)

    (org-list-dt         :foreground highlight)
    (org-meta-line       :foreground doc-comments)
    (org-priority        :foreground red)
    (org-property-value  :foreground doc-comments)
    (org-quote           :background base3 :slant 'italic :extend t)
    (org-special-keyword :foreground doc-comments)
    (org-table           :foreground violet)
    (org-tag             :foreground doc-comments :weight 'normal)
    (org-ref-cite-face   :foreground yellow :weight 'light :underline t)
    (org-latex-and-related :foreground base8 :weight 'bold)
    (org-todo            :foreground green :bold 'inherit)
    (org-verbatim        :foreground green)
    (org-warning         :foreground warning)

    ;; org-agenda
    (org-agenda-done :inherit 'org-done)
    (org-agenda-dimmed-todo-face :foreground comments)
    (org-agenda-date          :foreground violet :weight 'ultra-bold)
    (org-agenda-date-today    :foreground (lazycat-lighten violet 0.4)   :weight 'ultra-bold)
    (org-agenda-date-weekend  :foreground (lazycat-darken violet 0.4)  :weight 'ultra-bold)
    (org-agenda-structure     :foreground fg :weight 'ultra-bold)
    (org-agenda-clocking      :background (lazycat-blend blue bg 0.2))
    (org-upcoming-deadline         :foreground (lazycat-blend fg bg 0.8))
    (org-upcoming-distant-deadline :foreground (lazycat-blend fg bg 0.5))
    (org-scheduled            :foreground fg)
    (org-scheduled-today      :foreground base7)
    (org-scheduled-previously :foreground base8)
    (org-time-grid            :foreground comments)
    (org-sexp-date            :foreground fg)

    ;; org-habit
    (org-habit-clear-face          :weight 'bold :background base4)
    (org-habit-clear-future-face   :weight 'bold :background base3)
    (org-habit-ready-face          :weight 'bold :background (lazycat-blend blue bg-alt 0.5))
    (org-habit-ready-future-face   :weight 'bold :background (lazycat-blend blue bg-alt 0.3))
    (org-habit-alert-face          :weight 'bold :background (lazycat-blend yellow bg-alt 0.5))
    (org-habit-alert-future-face   :weight 'bold :background (lazycat-blend yellow bg-alt 0.3))
    (org-habit-overdue-face        :weight 'bold :background (lazycat-blend red bg-alt 0.5))
    (org-habit-overdue-future-face :weight 'bold :background (lazycat-blend red bg-alt 0.3))

    ;; org-journal
    (org-journal-highlight :foreground highlight)
    (org-journal-calendar-entry-face :foreground magenta :slant 'italic)
    (org-journal-calendar-scheduled-face :foreground red :slant 'italic)

    ;; org-pomodoro
    (org-pomodoro-mode-line :foreground red)
    (org-pomodoro-mode-line-overtime :foreground warning :weight 'bold)

    ;; rst-mode
    (rst-block :inherit 'font-lock-constant-face)
    (rst-level-1 :inherit 'rst-adornment :weight 'bold)
    (rst-level-2 :inherit 'rst-adornment :weight 'bold)
    (rst-level-3 :inherit 'rst-adornment :weight 'bold)
    (rst-level-4 :inherit 'rst-adornment :weight 'bold)
    (rst-level-5 :inherit 'rst-adornment :weight 'bold)
    (rst-level-6 :inherit 'rst-adornment :weight 'bold)

    ;; typescript-mode
    (typescript-jsdoc-tag :foreground doc-comments)
    (typescript-jsdoc-type :foreground (lazycat-darken doc-comments 0.15))
    (typescript-jsdoc-value :foreground (lazycat-lighten doc-comments 0.15))

    ;; sh-mode
    (sh-heredoc :inherit 'font-lock-string-face :weight 'normal)
    (sh-quoted-exec :inherit 'font-lock-preprocessor-face)

    ;; web-mode
    (web-mode-doctype-face           :foreground comments)
    (web-mode-html-tag-face          :foreground methods)
    (web-mode-html-tag-bracket-face  :foreground methods)
    (web-mode-html-attr-name-face    :foreground type)
    (web-mode-html-entity-face       :foreground cyan :inherit 'italic)
    (web-mode-block-control-face     :foreground orange)
    (web-mode-html-tag-bracket-face  :foreground operators)

    ;; woman
    (woman-bold :inherit 'Man-overstrike)
    (woman-italic :inherit 'Man-underline))
  "TODO")


(provide 'lazycat-theme)
