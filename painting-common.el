;;; painting-common.el --- total faces -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defgroup painting-theme nil
  "Painting-theme options."
  :group 'faces)

(defcustom painting-theme-custom-colors nil
  "Specify a list of custom colors."
  :type 'alist
  :group 'painting-theme)

(defun true-color-p ()
  "True color."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun create-painting-theme (variant theme-name)
  "Custom painting theme. THEME-NAME and VARIANT."
  (let ((class '((class color) (min-colors 89)))
        ;;                                      ~~ Dark ~~    ~~ Light ~~
        ;; generic
        (bg1              (if (eq variant 'dark) "#171717"    "#ffffff"))
        (bg2              (if (eq variant 'dark) "#393b44"    "#f4f4f4"))
        (bg3              (if (eq variant 'dark) "#414453"    "#e5e5e5"))
        (bg4              (if (eq variant 'dark) "#414453"    "#b4d8fd"))
        (bg5              (if (eq variant 'dark) "#2f3037"    "#ecf5ff"))
        (bg6              (if (eq variant 'dark) "#dfdfe0"    "#262626"))

        (fg1              (if (eq variant 'dark) "#afafaf"    "#262626"))
        (fg2              (if (eq variant 'dark) "#ff7ab2"    "#ad3da4"))
        (fg3              (if (eq variant 'dark) "#7f8c98"    "#8a99a6"))
        (fg4              (if (eq variant 'dark) "#b281eb"    "#804fb8"))
        (fg5              (if (eq variant 'dark) "#4eb0cc"    "#0f68a0"))
        (fg6              (if (eq variant 'dark) "#d9c97c"    "#272ad8"))
        (fg7              (if (eq variant 'dark) "#6bdfff"    "#0b4f79"))
        (fg8              (if (eq variant 'dark) "#ffa14f"    "#78492a"))
        (fg9              (if (eq variant 'dark) "#ff8170"    "#d12f1b"))
        (fg10             (if (eq variant 'dark) "#dfdfe0"    "#262626"))
        (fg11             (if (eq variant 'dark) "#292a30"    "#ffffff"))

        (main             (if (eq variant 'dark) "#8b8b8b"    "#8b8b8b"))

        (black-1          (if (eq variant 'dark) "#171717"    "#171717"))
        (black-2          (if (eq variant 'dark) "#242424"    "#242424"))
        (black-3          (if (eq variant 'dark) "#303030"    "#303030"))
        (black-4          (if (eq variant 'dark) "#323232"    "#323232"))
        (black-5          (if (eq variant 'dark) "#404040"    "#404040"))
        (ash              (if (eq variant 'dark) "#7c7c7c"    "#7c7c7c"))
        (gray             (if (eq variant 'dark) "#afafaf"    "#afafaf"))
        (pink             (if (eq variant 'dark) "#ff79c6"    "#ff79c6"))
        (orange           (if (eq variant 'dark) "#FC9F4E"    "#FC9F4E"))
        (red              (if (eq variant 'dark) "#E24C49"    "#E24C49"))
        (white            (if (eq variant 'dark) "#E0E0E0"    "#E0E0E0"))
        (yellow           (if (eq variant 'dark) "#CFA300"    "#CFA300"))
        (purple           (if (eq variant 'dark) "#B762DE"    "#B762DE"))
        (cyan             (if (eq variant 'dark) "#8BE9FD"    "#8BE9FD"))
        (green            (if (eq variant 'dark) "#39BA7E"    "#39BA7E"))

        (lightblue        (if (eq variant 'dark) "#96cbfe"    "#96cbfe"))
        (lightgreen       (if (eq variant 'dark) "#50FA7B"    "#50FA7B"))
        (lightorange      (if (eq variant 'dark) "#ffb86c"    "#ffb86c"))
        (lightpurple      (if (eq variant 'dark) "#BD93F9"    "#BD93F9"))

        (deepcyan         (if (eq variant 'dark) "#009F9F"    "#009F9F"))
        )

    (cl-loop for (cvar . val) in painting-theme-custom-colors
             do (set cvar val))

    (custom-theme-set-faces
     theme-name

;;;;; basics
     `(default ((,class (:background ,bg1 :foreground ,gray))))                  ;; 默认主题颜色
     `(default-italic ((,class (:italic t))))                                    ;; 斜体
     `(error ((,class (:foreground ,fg9))))                                      ;; 错误
     `(font-lock-builtin-face ((,class (:foreground ,gray :weight semibold))))   ;; 内建函数
     `(font-lock-comment-face ((,class (:foreground ,ash :italic t))))     ;; 注释
     `(font-lock-comment-delimiter-face ((,class (:foreground ,ash :italic t)))) ;; 注释符号
     `(font-lock-constant-face ((,class (:foreground ,deepcyan))))               ;; 常量
     `(font-lock-doc-face ((,class (:foreground ,deepcyan :italic t))))    ;; 文档
     `(font-lock-function-name-face ((,class (:foreground ,gray))))              ;; 函数名
     `(font-lock-keyword-face ((,class (:foreground ,lightpurple :weight semibold))))  ;; 关键字
     `(font-lock-negation-char-face ((,class (:foreground ,fg6))))               ;; 否定
     `(font-lock-preprocessor-face ((,class (:foreground ,deepcyan))))           ;; 预处理
     `(font-lock-reference-face ((,class (:foreground ,fg6))))                   ;; 参考
     `(font-lock-string-face ((,class (:foreground ,orange))))                   ;; 字符串
     `(font-lock-type-face ((,class (:foreground ,gray))))                       ;; 类型
     `(font-lock-variable-name-face ((,class (:foreground ,gray))))              ;; 变量
     `(font-lock-warning-face ((,class (:foreground ,red :background ,bg1))))    ;; 警告

     `(cursor ((,class (:background ,white))))                                     ;; 光标颜色
     `(fringe ((,class (:background nil  :foreground nil))))                     ;; 边缘
     `(region ((,class (:background ,black-4))))
     `(header-line ((,class (:background ,black-1 :foreground ,white))))
     `(highlight ((,class (:foreground ,fg1 :background ,bg4))))                 ;; 高亮
     `(hl-line ((,class (:background ,black-2 :extend t))))
     `(link ((,class (:foreground ,fg3 :underline t))))                          ;; 链接
     `(link-visited ((,class (:foreground ,fg3 :underline t))))                  ;; 已访问的链接
     `(match ((,class (:background ,black-3))))                                  ;; 匹配
     `(minibuffer-prompt ((,class (:inherit semibold :foreground ,fg2))))        ;; 提示
     `(warning ((,class (:foreground ,fg8))))                                    ;; 警告
     `(vertical-border ((,class (:foreground ,black-1))))
     `(button ((,class (:foreground "#2299cc" :underline t))))
     `(window-divider ((,class (:foreground ,black-5))))
     `(window-divider-first-pixel ((,class (:foreground ,black-2))))
     `(window-divider-last-pixel ((,class (:foreground ,black-2))))
     `(line-number ((,class (:foreground ,yellow))))
     `(line-number-current-line ((,class (:background ,black-2 :foreground ,yellow))))
     `(lazy-highlight ((,class (:background ,green :foreground ,gray))))
     `(which-func ((,class (:foreground ,gray))))

;;;;; mode-line
     `(mode-line           ((,class (:background ,black-4))))
     `(mode-line-buffer-id ((,class (:foreground ,fg4))))
     `(mode-line-inactive  ((,class (:foreground ,fg1 :background ,bg3  :box (:color ,fg11 :line-width 1)))))

;;;;; mood-line
     `(mood-line-buffer-name ((,class (:foreground ,fg1))))
     `(mood-line-major-mode ((,class (:foreground ,fg1))))

;;;;; tab-bar-mode
     `(tab-bar ((,class (:foreground ,fg1 :background ,bg1))))
     `(tab-bar-tab ((,class (:foreground ,fg1 :background ,bg1 :weight bold))))
     `(tab-bar-tab-inactive ((,class (:foreground ,fg1 :background ,bg3 :weight light))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; company-mode
     `(company-tooltip ((,class (:background ,bg2 :foreground ,fg1))))
     `(company-tooltip-common ((,class (:background ,bg2 :foreground ,fg1))))
     `(company-tooltip-common-selection ((,class (:foreground ,fg1))))
     `(company-tooltip-selection ((,class (:background ,bg4 :foreground ,fg1))))
     `(company-tooltip-scrollbar-track ((,class (:background ,bg2))))
     `(company-tooltip-scrollbar-thumb ((,class (:background ,bg3))))
     `(company-tooltip-annotation ((,class (:foreground ,fg3))))

;;;;; company-coq
     `(company-coq-snippet-hole-face ((,class (:weight light :italic nil))))

;;;;; ivy
     `(ivy-current-match ((,class (:background ,bg4 :extend t))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit normal))))
     `(ivy-minibuffer-match-face-2 ((,class (:inherit normal))))
     `(ivy-minibuffer-match-face-3 ((,class (:inherit normal))))
     `(ivy-minibuffer-match-face-4 ((,class (:inherit normal))))
     `(ivy-modified-buffer ((,class (:foreground ,fg1))))
     `(ivy-virtual ((,class (:foreground ,fg1))))
     `(ivy-subdir ((,class (:foreground ,fg1))))
     `(ivy-remote ((,class (:foreground ,fg1))))

;;;;; swiper
     `(swiper-line-face ((,class (:background ,bg4))))
     `(swiper-match-face-1 ((,class (:inherit normal))))
     `(swiper-match-face-2 ((,class (:inherit normal))))
     `(swiper-match-face-3 ((,class (:inherit normal))))
     `(swiper-match-face-4 ((,class (:inherit normal))))

;;;;; proof-general
     `(proof-locked-face ((,class (:background ,bg5))))
     `(coq-solve-tactics-face  ((,class (:foreground ,fg4))))
     `(proof-tacticals-name-face ((,class (:foreground ,fg4))))
     `(proof-tactics-name-face ((,class (:foreground ,fg4))))
     `(coq-cheat-face ((,class (:foreground ,fg9))))
     `(proof-declaration-name-face ((,class (:foreground ,fg5))))
     `(coq-button-face ((,class (:ground ,bg1))))
     )))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'painting-common)
;;; painting-common.el ends here
