(require 'cl-lib)

(deftheme lazycat "A dark theme inspired by Atom One Dark")

(defvar lazycat-themes--colors nil)

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
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(let*
    ((bg             "#242525")
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
   `(bold ((t (:weight bold :foreground ,(unless bold base8)))))
   '(italic ((t (:slant italic))))
   `(default ((t (:background ,bg :foreground ,fg))))
   `(fringe ((t (:inherit default :foreground ,base4))))

   `(region ((t (:background ,region :foreground ,region-fg))))
   `(highlight ((t (:background ,highlight :foreground ,base0 :distant-foreground ,base8))))
   `(cursor ((t (:background ,highlight))))
   `(shadow ((t (:foreground ,base5))))
   `(minibuffer-prompt ((t (:foreground ,highlight))))
   `(tooltip ((t (:background ,base3 :foreground ,fg))))
   `(secondary-selection ((t (:background ,grey :extend t))))
   `(lazy-highlight ((t (:background ,dark-blue :foreground ,base8 :distant-foreground ,base0 :weight bold))))
   `(match ((t (:foreground ,green :background ,base0 :weight bold))))
   `(trailing-whitespace ((t (:background ,red))))
   `(nobreak-space ((t (:inherit default :underline nil))))
   `(vertical-border ((t (:background ,vertical-bar :foreground ,vertical-bar))))
   `(link ((t (:foreground ,highlight :underline t))))
   `(error ((t (:foreground ,error))))
   `(warning ((t (:foreground ,warning))))
   `(success ((t (:foreground ,success))))

   ))



(custom-theme-set-variables
 'lazycat

 `(ansi-color-names-vector
   (vconcat (mapcar #'lazycat-color '(bg red green yellow blue magenta cyan fg))))

 `(rustic-ansi-faces
   (vconcat (mapcar #'lazycat-color '(bg red green yellow blue magenta cyan fg))))

 `(fci-rule-color ,(lazycat-color 'base5))
 `(objed-cursor-color ,(lazycat-color 'red))
 `(pdf-view-midnight-colors (cons ,(lazycat-color 'fg) ,(lazycat-color 'bg)))

 `(vc-annotate-color-map
   (list (cons 20  ,(lazycat-color 'green))
         (cons 40  ,(lazycat-blend 'yellow 'green (/ 1.0 3)))
         (cons 60  ,(lazycat-blend 'yellow 'green (/ 2.0 3)))
         (cons 80  ,(lazycat-color 'yellow))
         (cons 100 ,(lazycat-blend 'orange 'yellow (/ 1.0 3)))
         (cons 120 ,(lazycat-blend 'orange 'yellow (/ 2.0 3)))
         (cons 140 ,(lazycat-color 'orange))
         (cons 160 ,(lazycat-blend 'magenta 'orange (/ 1.0 3)))
         (cons 180 ,(lazycat-blend 'magenta 'orange (/ 2.0 3)))
         (cons 200 ,(lazycat-color 'magenta))
         (cons 220 ,(lazycat-blend 'red 'magenta (/ 1.0 3)))
         (cons 240 ,(lazycat-blend 'red 'magenta (/ 2.0 3)))
         (cons 260 ,(lazycat-color 'red))
         (cons 280 ,(lazycat-blend 'grey 'red (/ 1.0 4)))
         (cons 300 ,(lazycat-blend 'grey 'red (/ 2.0 4)))
         (cons 320 ,(lazycat-blend 'grey 'red (/ 3.0 4)))
         (cons 340 ,(lazycat-color 'base5))
         (cons 360 ,(lazycat-color 'base5))))
 `(vc-annotate-very-old-color nil)
 `(vc-annotate-background ,(lazycat-color 'bg)))

;; (set-face-bold 'bold nil)
;; (set-face-italic 'italic nil)

(provide-theme 'lazycat)
