;; (defun copy-selected-text (start end)
;;   (interactive "r")
;;     (if (use-region-p)
;;	   (let ((text (buffer-substring-no-properties start end)))
;;	       (shell-command (concat "echo '" text "' | clip.exe")))))
;; ; wsl-copy
;; (defun wsl-copy (start end)
;;   (interactive "r")
;;   (shell-command-on-region start end "clip.exe")
;;   (deactivate-mark))

;; ; wsl-paste
;; (defun wsl-paste ()
;;   (interactive)
;;   (let ((clipboard
;;	(shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
;;     (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
;;     (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
;;     (insert clipboard)))

;; (setq select-enable-clipboard nil)

					; Bind wsl-copy to C-c C-v
;; (global-set-key
;;  (kbd "<C-Insert>")
;;  'copy-selected-text)

; Bind wsl-paste to C-c C-v
;; (global-set-key
;;  (kbd "<S-Insert>")
;;  'wsl-paste)


(defun php-swap-quotes (start end)
  (interactive "r")
  (if (use-region-p)
      (progn
	(deactivate-mark)
	(goto-char start)
	(let ((first-found-char nil)
	      (found-char nil)
	      (replace-char nil)
	      (new-end end))
	  (while (re-search-forward "[\"'\{\}]" new-end t)
	      (setq found-char (char-to-string (preceding-char)))
	      (setq replace-char (if (string-match "\"" found-char) "'"
				   (if (string-match "'" found-char) "\""
				     (progn
				       (setq new-end (+ new-end 2))
				       (if (string-match "{" found-char)
					   (concat first-found-char ".")
					 (concat "." first-found-char)
					 ))
				     )
				   ))
	      (if (not first-found-char) (setq first-found-char replace-char))
	      (backward-char 1)
	      (delete-char 1)
	      (insert replace-char)
	    )
	  )
	)
    )
  )

;; turn off truncate lines in grep mode
(add-hook 'grep-mode-hook #'(lambda () (toggle-truncate-lines nil)))

(defun my-rgrep ()
  "Shortcut to rgrep with specific defaults"
  (interactive)
  (grep-compute-defaults)
  (rgrep (grep-read-regexp) "*.py *.html" "./")
  (toggle-truncate-lines 1))

;; upcase and downcase region without the prompt
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun yoda-reverse (start end)
  (interactive "r")
  (if (use-region-p)
      (insert
       (mapconcat 'identity
		  (nreverse
		   (split-string
		    (delete-and-extract-region start end) " ")) " "))))


;; ----------------------------------------------------------------
;; after-save-hook functions
(defun my-after-save-hooks ()
  "My after-save-hooks."
  (progn
    (autocompile-emacs-file)
    (prune-buffers)
    )
  )

(defun autocompile-emacs-file ()
  "Compiles the file if it is ~/.emacs or ~/.emacs.funcs."
  (interactive)
  (let ((name (buffer-name (car (buffer-list (frame-first-window))))))
    (if (or (string= name ".emacs")
	    (string= name ".emacs.funcs")
	    (string= name "init.el")
	    (string= name "init.funcs.el")
	    (string= name "init.display.el"))
	(byte-compile-file (buffer-file-name)))))

(defvar prune-buffers-flag nil "True when buffers are pruned.")
(defun prune-buffers ()
  "Prunes the *<buffer>* buffers."
  (interactive)
  (setq prune-buffers-flag nil)
  (prune-buffer-helper (buffer-list (frame-first-window)))
  (when prune-buffers-flag
    (message "Some buffers were pruned.")
    (sit-for 1 nil)))

(defun prune-buffer-helper (l)
  (cond
   ((null l) t)
   (t (let* ((buffer (car l))
	     (name (buffer-name buffer)))
	(when (and (string= (substring name 0 1) "*")
		   (not (string= name "*scratch*"))
		   (not (string= name "*Compile-Log*")))
	  (kill-buffer buffer)
	  (if (not (string= name "*scratch*"))
	      (setq prune-buffers-flag t)))
	(prune-buffer-helper (cdr l))))))


(defvar buffer-last-change-time nil "The last change time")
(make-variable-buffer-local 'buffer-last-change-time)

(defun buffer-record-last-change-time (beg end len)
  (setq buffer-last-change-time (current-time)))

(add-to-list 'after-change-functions 'buffer-record-last-change-time)

(defun buffer-last-modified ()
  (interactive)
  "get last modified time for buffer"
  (message (format-time-string "Recorded last change time as %F %T"
			       buffer-last-change-time))
  nil)

(defun backup-file ()
  (interactive)
  "create a backup file with the ext of .bak"
  (let ((backupfile (concat buffer-file-name ".bak")))
    (if (file-exists-p backupfile)
	(if (y-or-n-p (format "Overwrite %S? " backupfile))
	    (progn
	      (copy-file buffer-file-name backupfile t)
	      (message (concat "updated backup: " backupfile)))
	  (message "backup not taken"))
      (progn
	(copy-file buffer-file-name backupfile)
	(message (concat "created backup: " backupfile)))))
  nil)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun reload-file ()
    (interactive)
    "revert modifiied buffer without confirmation."
    (if (not (buffer-modified-p))
	(progn
	  (revert-buffer :ignore-auto :noconfirm)
	  (message "buffer reloaded"))
      (if (y-or-n-p "Buffer modified, reload?")
	(progn
	  (revert-buffer :ignore-auto :noconfirm)
	  (message "buffer reloaded"))
	)
      )
    )

;; -----------------------------------------------------------------
;; interactive buffer modification functions (often used as write-file-hooks)

(defun my-write-file-hooks ()
  "My write-file-hooks.
    (has to always return nil, since it is added to write-file-hooks)"
  (tabify-buffer)
  ;; (untabify-buffer)
  (prune-spaces)
;  (dosify-buffer)
;  (unixify-buffer)
  nil)


(defun strip-unicode ()
  "Convert common unicode characters."
  (interactive)
  (save-excursion
    (set-buffer-file-coding-system 'undecided-dos)
    (goto-char (point-min))
    (while (re-search-forward "[\u2013\u2014\u2015]" nil t)
      (replace-match "-" t nil))
    (goto-char (point-min))
    (while (re-search-forward "[\u2018\u2019\u201B]" nil t)
      (replace-match "'" t nil))
    (goto-char (point-min))
    (while (re-search-forward "[\u201C\u201D]" nil t)
      (replace-match "\"" t nil))
    ))


(defun tabify-buffer ()
  (interactive)
  "Tabifies the current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; delete empty lines
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    ;; replace with tabs only at beginning of line
    (while (re-search-forward "^[ \t]+" nil t)
      (progn
	(print (match-beginning 0))
	(print (match-end 0))
	(tabify (match-beginning 0) (match-end 0))
	))))

(defun untabify-buffer ()
  (interactive)
  "Untabifies the current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; delete empty lines
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    ;; remove all tabs everywhere
    (if (search-forward "\t" nil t)
	(untabify (1- (point)) (point-max)))))

(defvar pruned-spaces-flag nil "True when spaces were pruned.")

(defun prune-spaces ()
  "Prunes extra spaces at the end of a line."
  (interactive)
  (prune-spaces-helper)
  (when pruned-spaces-flag
    (message "Some extraneous spaces were deleted.")
    (sit-for 1 nil)))

(defun prune-spaces-helper ()
  (setq pruned-spaces-flag nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "")
      (setq pruned-spaces-flag t))))

(defun unixify-buffer ()
  "Convert buffer to unix file format."
  (interactive)
  (save-excursion
    (set-buffer-file-coding-system 'undecided-unix)
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun dosify-buffer ()
  "Convert buffer to dos file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos))

(defun unwrap-lines()
  "Unwraps the lines of a buffer."
  (interactive)
  (prune-spaces-helper)
  (let ((unwrap-lines-flag nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
	(if (char-equal ?\n (following-char))
	    (while (char-equal ?\n (following-char))
	      (forward-char))
	  (progn (replace-match " ")
		 (setq unwrap-lines-flag t))))
      (when unwrap-lines-flag
	(message "Some lines were unwrapped.")))))

(defun extract-regexp (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*extract-regexp*")))
    (with-current-buffer result-buffer (erase-buffer))
    (save-match-data
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (princ
	   (concat
	    (buffer-substring-no-properties (match-beginning 0)
					    (match-end 0)) "\n")
		 result-buffer))))
    (pop-to-buffer result-buffer)))


;; ----------------------------------------------------------------
;; display modes
(defun line-truncate ()
  "Toggles line truncation."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (if truncate-lines
      (message "Truncate lines is off.")
    (message "Truncate lines is on."))
  (sit-for 1 nil))

;; (defun which-func ()
;;   "Toggles which-func mode."
;;   (interactive)
;;   (setq which-function-mode (not which-function-mode))
;;   (if which-function-mode
;;	 (message "Which-func mode is on.")
;;     (message "Which-func mode is off."))
;;   (sit-for 1 nil))

(defun word-count (start end)
  (interactive "r")
  (let ((chars 0)
	(words 0)
	(lines 0)
	(in-word nil))
    (while (< start end)
      (cl-incf chars)
      (cond ((= (char-after start) ?\n)
	     (setq in-word nil)
	     (cl-incf lines))
	    ((= (char-syntax (char-after start)) ?\ )
	     (setq in-word nil))
	    ((not in-word)
	     (cl-incf words)
	     (setq in-word t)))
      (cl-incf start))
    (message "%d Characters %d Words %d Lines" chars words lines)
    (list chars words lines)))

;; -----------------------------------------------------------------
;; editing modes


(defun shift-mouse-select (event)
  "Set the mark and then move point to the position clicked on with
    the mouse. This should be bound to a mouse click event type."
  (interactive "e")
  (mouse-minibuffer-check event)
  (if mark-active (exchange-point-and-mark))
  (set-mark-command nil)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (posn-set-point (event-end event)))

(defun goto-matching-paren()
  "Jump to the matching parenthesis."
  (interactive)
  (let ((point-char-syntax (char-syntax (char-after))))
    (cond
     ((eq point-char-syntax (char-syntax ?\())
      (progn
       (forward-sexp)
       (backward-char)))
     ((eq point-char-syntax (char-syntax ?\)))
      (progn
	(forward-char)
	(backward-sexp)))
     (t (error "Character at point is not at a parenthesis.")))))


(defvar greedy-clear-flag t "True when greedy clear is activated.")

;; (defun greedy-clear ()
;;   "Toggles greedy clear."
;;   (interactive)
;;   (setq greedy-clear-flag (not greedy-clear-flag))
;;   (if greedy-clear-flag
;;	 (message "Greedy clear is on.")
;;     (message "Greedy clear is off.")))

(defun non-greedy-backsp ()
  "Regular clear backspace."
  (interactive)
  (if mark-active
      (delete-region (region-beginning) (region-end))
    (delete-char -1)))

(defun greedy-backsp ()
  "Greedy clear backspace"
  (interactive)
  (progn (non-greedy-backsp)
	 (if greedy-clear-flag
	     (let ((end (point)))
	       (skip-chars-backward " \t\n")
	       (if (/= (point) end)
		   (delete-region (point) end))))))

(defun non-greedy-delete ()
  "Regular clear delete."
  (interactive)
  (if mark-active
      (delete-region (region-beginning) (region-end))
    (delete-char 1)))

(defun greedy-delete ()
  "Greedy clear delete"
  (interactive)
  (progn (non-greedy-delete)
	 (if greedy-clear-flag
	     (let ((start (point)))
	       (skip-chars-forward " \t\n")
	       (if (/= start (point))
		   (delete-region start (point)))))))
