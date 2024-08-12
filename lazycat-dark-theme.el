;;; lazycat-dark-theme.el --- My dark theme, for night use.

;; Filename: lazycat-dark-theme.el
;; Description: My dark theme, for night use.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-21 15:00:25
;; Version: 0.1
;; Last-Updated: 2020-03-21 15:00:25
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/lazycat-dark-theme.el
;; Keywords:
;; Compatibility: GNU Emacs 26.3
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; My dark theme, for night use.
;;

;;; Installation:
;;
;; Put lazycat-dark-theme.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lazycat-dark-theme)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET lazycat-dark-theme RET
;;

;;; Change log:
;;
;; 2020/03/21
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:

(require 'lazycat-theme)

(defgroup lazycat-dark-theme nil
  "Options for lazycat-themes"
  :group 'lazycat-themes)

(def-lazycat-theme lazycat-dark
  "A dark theme inspired by Atom One Dark"

  ((bg         "#242525")
   (bg-alt     "#333333")
   (base0      "#1B2229")
   (base1      "#1c1f24")
   (base2      "#202328")
   (base3      "#23272e")
   (base4      "#3f444a")
   (base5      "#5B6268")
   (base6      "#73797e")
   (base7      "#9ca0a4")
   (base8      "#DFDFDF")
   (fg         "#00CE00")
   (fg-alt     "green4")

   (grey       base4)
   (red        "#ff6c6b")
   (orange     "#da8548")
   (green      "#98be65")
   (teal       "#4db5bd")
   (yellow     "#ECBE7B")
   (blue       "#51afef")
   (dark-blue  "#2257A0")
   (magenta    "#c678dd")
   (violet     "#a9a1e1")
   (cyan       "#46D9FF")
   (dark-cyan  "#5699AF")

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
   (hidden     `(,(car bg) "black" "black"))))

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

;;; lazycat-dark-theme.el ends here
