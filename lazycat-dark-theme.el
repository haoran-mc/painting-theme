(require 'lazycat-theme)


(def-lazycat-theme lazycat-dark
  "A dark theme inspired by Atom One Dark"

  ((bg             "#242525")
   (bg-alt         "#333333")
   (base0          "#1B2229")
   (base1          "#1c1f24")
   (base2          "#202328")
   (base3          "#23272e")
   (base4          "#3f444a")
   (base5          "#5B6268")
   (base6          "#73797e")
   (base7          "#9ca0a4")
   (base8          "#DFDFDF")
   (fg             "#00CE00")
   (fg-alt         "green4")

   (grey           base4)
   (red            "#ff6c6b")
   (orange         "#da8548")
   (green          "#98be65")
   (teal           "#4db5bd")
   (yellow         "#ECBE7B")
   (blue           "#51afef")
   (dark-blue      "#2257A0")
   (magenta        "#c678dd")
   (violet         "#a9a1e1")
   (cyan           "#46D9FF")
   (dark-cyan      "#5699AF")

   ;; face categories -- required for all themes
   (highlight      "green")
   (vertical-bar   (lazycat-darken base1 0.1))
   (selection      dark-blue)
   (builtin        "#00b8ff")
   (comments       "#a7a7a7")
   (doc-comments   "#aaaaaa")
   (constants      "#bd00ff")
   (functions      "gold2")
   (keywords       "#004FFF")
   (methods        cyan)
   (operators      "cyan3")
   (type           "#00b8ff")
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
   (hidden         bg)))

(custom-theme-set-variables
 'lazycat-dark

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


(provide 'lazycat-dark-theme)
