;-------------------------[ Version Check ]-------------------------;
(defvar my-version "the current version")
(setq my-version 0); initialize the version flag
(if(string-match "GNU Emacs 26" (version))
    (setq my-version 26)
  (if(string-match "GNU Emacs 24" (version))
      (setq my-version 24)
    (if(string-match "GNU Emacs 23" (version))
  (setq my-version 23)
      (if(string-match "GNU Emacs 22" (version))
    (setq my-version 22)
  (if(string-match "GNU Emacs 21" (version))
      (setq my-version 21)
    (if(string-match "GNU Emacs 20" (version))
        (setq my-version 20)))))))


;---------------------------[ Display Settings ]---------------------------;

;; use alt-x 'describe char' to figure out what is being shown
;; color reference: http://www.raebear.net/computers/emacs-colors/

(defvar my-color-def-f        "gainsboro"         "default foreground")
(defvar my-color-def-b        "grey20"            "default background")
(defvar my-color-hi           "steel blue"  "highlight background")
(defvar my-color-func         "dark salmon"        "function foreground")
;(defvar my-color-comm         "gray40"            "comment face")
(defvar my-color-comm         "slate gray"        "comment face")
;(defvar my-color-kwc          "light slate gray"   "keyword-const")
(defvar my-color-kwc          "cornflower blue"   "keyword-const")
(defvar my-color-str          "spring green"      "string")
(defvar my-color-err          "firebrick1"      "errors")
(defvar my-color-warn         "gold1"      "warnings")
(defvar my-color-info         "slate gray"      "info")

(if(>= my-version 21)
    (progn
      ;; (set-face-attribute 'cursor nil :background my-color-def-b :foreground my-color-def-f :weight 'bold)
      (set-face-attribute 'cursor nil :background my-color-def-b :foreground my-color-def-f)
      (set-face-background 'cursor                  my-color-def-f)
      (set-face-background 'fringe                  my-color-def-b)
      (set-face-foreground 'isearch                 my-color-def-b)
      (set-face-background 'isearch                 "pale violet red")
      (set-face-foreground 'linum                   my-color-comm)
      (set-face-background 'linum                   my-color-def-b)
      ;(set-face-foreground 'line-number                      my-color-comm)
      ;(set-face-background 'line-number                      my-color-def-b)
      ;(set-face-foreground 'line-number-current-line         my-color-comm)
      ;(set-face-background 'line-number-current-line         my-color-def-b)

      )
  (progn
    (add-to-list 'default-frame-alist `(foreground-color . my-color-def-f))
    (add-to-list 'default-frame-alist `(background-color . my-color-def-b))
    (add-to-list 'default-frame-alist `(cursor-color . my-color-def-f))))

(set-face-attribute 'default nil
                    :background my-color-def-b
                    :foreground my-color-def-f)
(set-face-attribute 'font-lock-builtin-face nil
                    :foreground my-color-kwc)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground my-color-comm
                    :weight 'bold
                    )
(set-face-attribute 'font-lock-comment-delimiter-face nil
                    :foreground my-color-comm)
(set-face-attribute 'font-lock-constant-face nil
                    :foreground my-color-kwc)
(set-face-attribute 'font-lock-doc-face nil
                    :foreground my-color-comm)
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground my-color-func
                    :weight 'bold)
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground my-color-kwc
                    :weight 'bold)
(set-face-attribute 'font-lock-negation-char-face nil
                    :foreground my-color-kwc)
(set-face-attribute 'font-lock-preprocessor-face nil
                    :foreground my-color-kwc)
(set-face-attribute 'font-lock-regexp-grouping-backslash nil
                    :foreground my-color-comm)
(set-face-attribute 'font-lock-regexp-grouping-construct nil
                    :foreground my-color-kwc)
(set-face-attribute 'font-lock-string-face nil
                    :foreground my-color-str)
(set-face-attribute 'font-lock-type-face nil
                    :foreground my-color-str
                    :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground my-color-def-f)
(set-face-attribute 'font-lock-warning-face nil
                    :foreground my-color-warn)
(set-face-attribute 'highlight  nil
                    :foreground my-color-def-b)

(set-face-background 'highlight                        my-color-hi)
(set-face-foreground 'minibuffer-prompt                my-color-def-f)
(set-face-foreground 'region                           my-color-def-b)
(set-face-background 'region                           my-color-hi)

(with-eval-after-load 'flycheck
  (set-face-attribute 'flycheck-error nil :underline '(:color "firebrick1" :style wave))
  (set-face-foreground 'flycheck-fringe-error my-color-err)
  (set-face-attribute 'flycheck-warning nil :underline '(:color "gold1" :style wave))
  (set-face-foreground 'flycheck-fringe-warning my-color-warn)
;; (set-face-foreground 'flycheck-info my-color-info)
;; (set-face-foreground 'flycheck-fringe-info my-color-info)

  )

(if(<= my-version 24)
    (progn
      (set-face-foreground 'isearch-lazy-highlight-face  my-color-def-b)
      (set-face-background 'isearch-lazy-highlight-face  my-color-hi)
      (set-face-background 'show-paren-match-face        "green")
      (set-face-background 'show-paren-mismatch-face     "red")))

(if(>= my-version 24)
    (progn
      (set-face-foreground 'mode-line                    my-color-def-f)
      (set-face-background 'mode-line                    my-color-comm))
  (progn
    (set-face-foreground 'modeline                    my-color-def-f)
    (set-face-background 'modeline                    my-color-comm))
  )


;; (if(>= my-version 22)
;;     (progn
;;       (setq w32-enable-synthesized-fonts t)))

(defvar font-maker)
(defvar font-family)
(defvar font-width)
(defvar font-style)
(defvar font-height)
(defvar font-pixels)
(defvar font-spacing)
(if (memq window-system '(win32 w32))
    (progn
      (setq font-maker "outline")
      (setq font-family "Courier New")
      (setq font-width "normal")
      (setq font-style "normal")
      (setq font-spacing "c")
      (setq font-pixels "*")
      ; fix initial position
      (add-to-list 'default-frame-alist '(top . 40))
      (add-to-list 'default-frame-alist '(left . 80))
      ; fix window size
      (add-to-list 'default-frame-alist '(height . 36))
      (add-to-list 'default-frame-alist '(width . 80))

      (if (>= (x-display-pixel-width) 3200)
          (progn
            ;(set-face-attribute 'default nil :height 140)
            (setq font-height "60")
            )
        (if (>= (x-display-pixel-width) 1280)
            (progn
              (setq font-height "24")
              )
          (progn
            (setq font-height "16")
            )
          )
        )
      )
  (if (memq window-system '(x))
      (progn
        (setq font-maker "*")
        (setq font-family "Ubuntu Mono")
        (setq font-width "normal")
        (setq font-style "*")
        (setq font-spacing "*")
        (setq font-pixels "*")

        (if (<= (x-display-pixel-width) 1280)
            (progn
              (setq font-height "16")
              )
          (progn
            (setq font-height "20")
            )
          )
        )
    )
)

(defvar default-font-format "-%s-%s-normal-r-%s-%s-%s-%s-*-*-%s-*-iso10646-1")
(defvar default-font
      (format default-font-format font-maker font-family font-width
              font-style font-height font-pixels font-spacing))

;; (defvar bold-font-format "-%s-%s-bold-r-%s-%s-%s-%s-*-*-%s-*-iso8859-1")
;; (defvar bold-font
;;       (format bold-font-format font-maker font-family font-width
;;               font-style font-height font-pixels font-spacing))

;; (defvar bold-italic-font-format "-%s-%s-bold-i-%s-%s-%s-%s-*-*-%s-*-iso8859-1")
;; (defvar bold-italic-font
;;      (format bold-italic-font-format font-maker font-family font-width
;;              font-style font-height font-pixels font-spacing))

; set the fonts
(add-to-list 'default-frame-alist `(font . ,default-font))
;; (set-face-font 'font-lock-comment-face bold-italic-font)
;; (set-face-font 'font-lock-type-face bold-font)
;; (set-face-font 'font-lock-function-name-face bold-font)
;; (set-face-font 'font-lock-keyword-face bold-font)

; unix font
;;"-adobe-courier-medium-r-normal--*-180-*-*-m-*-iso8859-1"

;.Xresources
;emacs*background: DarkSlateGray
;emacs*foreground: Wheat
;emacs*pointerColor: Orchid
;emacs*cursorColor: Orchid
;emacs*bitmapIcon: on
;emacs*font-lock-comment-face*attributeItalic:    on
;emacs*font-lock-function-name-face*attributeBold:  on
;emacs*font-lock-keyword-face*attributeBold:    on
;emacs*default*attributeFont: -adobe-courier-medium-r-normal--*-180-*-*-m-*-iso8859-1
;emacs.geometry: 80x40

(make-face 'mode-line-buffer-face)
(set-face-foreground 'mode-line-buffer-face my-color-def-b)
;; (set-face-font 'mode-line-buffer-face bold-font)

;; (make-face 'mode-line-which-func-face)
;; (copy-face 'font-lock-function-name-face 'mode-line-which-func-face)
;; (set-face-background 'mode-line-which-func-face my-color-def-b)


;; Rearrange the modeline so that everything is to the left of the
;; long list of minor modes, which is relatively unimportant but takes
;; up so much room that anything to the right is obliterated.
(setq-default mode-line-buffer-identification
              '(:eval (propertize "%b" 'face 'mode-line-buffer-face)))  ;default ("%12b") which pads twelve chars

(setq-default mode-line-format
        (list "-"
        'mode-line-mule-info
        'mode-line-modified
        'mode-line-frame-identification
        'mode-line-buffer-identification
        '(isearch-case-fold-search " case-ins")
        '(line-number-mode " (L%l.")
        ;; '(-3 . "%p")
        '(column-number-mode "C%c) ")
        ;; '(which-function-mode ("" which-function-format " "))
        'global-mode-string
;;         "%[("
        'mode-name
        ;; 'mode-line-process
        ;; 'minor-mode-alist
;;         "%n"
;;         ")%]"
))
