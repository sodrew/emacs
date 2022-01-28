;; .emacs.d/init.el
;; (toggle-debug-on-quit)

;; (setq gc-cons-threshold 500000000)

;; add the path
(if (memq window-system '(win32 w32))
  (add-to-list 'exec-path (concat (getenv "HOME") "\\wbin")))

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; my-packages contains a list of package names
(defvar my-packages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    magit                           ;; Git integration
    disable-mouse                   ;; disable mouse inputs
    cypher-mode                     ;; for neo4j graph ql
    ;; material-theme                  ;; Theme
    ;; zenburn-theme
    )
  )

;; pip install pylint

;; Scans the list in my-Packages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      my-packages)

;; ===================================
;; Basic Customization
;; ===================================
(global-linum-mode t)          ;; Enable line numbers globally
(delete-selection-mode t)      ;; overwrite selected text
(menu-bar-mode -1)             ;; hides the menu bar
(tool-bar-mode -1)             ;; hides the tool bar
(column-number-mode t)         ;; adds column number to mode line
(show-paren-mode 1)            ;; match up the parens
(blink-cursor-mode 0)          ;; stops the blinking cursor
;(mouse-avoidance-mode 'jump)   ;; moves the mouse out of the way
(fset 'yes-or-no-p 'y-or-n-p)  ;; type 'y' instead of 'yes'
(standard-display-8bit 128 255);; display other chars
(standard-display-ascii ?\r "");; don't display the carriage return

(setq
 ;auto-save-default nil         ;; turn off autosave
 ;auto-save-list-file-prefix nil;; avoid save litter
 make-backup-files nil         ;; stop auto backup file creation
 auto-save-default t
 auto-save-timeout 10
 auto-save-interval 200
 auto-save-file-name-transforms `((".*" "~/backups/" t))
 ring-bell-function 'ignore    ;; turn off the beeps
 inhibit-startup-message t     ;; hide the startup message
 initial-scratch-message nil   ;; kill the initial scratch message
 frame-title-format "%b"       ;; file name in window title
 icon-title-format "!%b"       ;; file name in window title  when iconified
 initial-major-mode 'text-mode ;; sets the major mode to text
 insert-default-directory nil  ;; hides the default dir on fopen
 next-line-add-newlines nil    ;; no newlines on down key
 scroll-step 3                 ;; move only one line at a time
 scroll-conservatively 200     ;; scrolling
 mouse-wheel-scroll-amount '(3)
 mouse-wheel-progressive-speed nil
 )

;; load custom funcs
(setq custom-file "~/.emacs.d/init.funcs")
(load custom-file)

;; make our hooks functional
(add-hook 'after-save-hook 'my-after-save-hooks)
(add-hook 'write-file-hooks 'my-write-file-hooks)

;; load custom display colors
(setq custom-file "~/.emacs.d/init.display")
(load custom-file)

;; turn off python indent guess warnings
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; ===================================================================
;; Key Bindings
;; ===================================================================
(global-unset-key [(ctrl v)]); stop mistakenly jumping to the next screen when trying to paste

(global-unset-key [(ctrl l)]); turn off scrolling by line
(global-set-key [(ctrl l)] 'goto-line); goto line (when we aren't cancelling)

(global-unset-key [(ctrl h)])
(global-set-key [(ctrl h)] 'replace-string); replace-string
;; (global-unset-key [(ctrl j)]) ;;commented so that lisp eval in scratch works
(global-set-key [(ctrl j)] 'replace-regexp); replace-regexp

(global-set-key [f1] 'toggle-read-only); toggles the buffer read only
(global-unset-key [f2])
(global-set-key [f2] 'isearch-toggle-case-fold); toggles case sensitive search

;; (global-set-key [f5] 'my-run-some-commands)

;; (global-set-key [(shift f7)] 'kill-compilation)

(global-set-key [f12] 'line-truncate); toggles line truncate display

(global-set-key [(M-f4)] 'delete-frame); bind alt f4 to kill emacs

;; Don't show a stupid font context menu when shift-clicking a buffer.
;; (global-set-key [S-mouse-1] 'shift-mouse-select)
;; (global-set-key [S-down-mouse-1] 'ignore)
;; (global-set-key [C-mouse-1] 'ignore)
;; (global-set-key [C-down-mouse-1] 'ignore)

;; bind to function in .funcs file
(global-set-key "\C-xc" 'backup-file)
(global-unset-key [(ctrl p)])
(global-set-key [(ctrl p)] 'goto-matching-paren); match-paren
(global-unset-key [(ctrl k)])
(global-set-key [(ctrl k)] 'my-rgrep)
(global-set-key [(ctrl delete)] 'greedy-delete); greedy delete
(global-set-key [(ctrl backspace)] 'greedy-backsp); greedy backspace

;; ====================================
;; Development Setup
;; ====================================
(cd "/home/drew/desktop/dev/config/studyapp/management/commands")

;; Enable elpy
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Turn off mouse inputs
;; (when (require 'disable-mouse nil t)
;;   (global-disable-mouse-mode))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 158 :width normal))))
;;  '(font-lock-comment-face ((t (:background "gainsboro" :foreground "red" :weight bold)))))
;; )

;; ;; User-Defined init.el ends here
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages (quote (material-theme better-defaults))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
