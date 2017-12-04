;; ---- keybindings ----

(global-set-key [f1] 'dired);; run the dired command [f1]
(global-set-key [f4] 'goto-line);; goto line [f4]
(global-set-key [f5] 'compile);; run compile [f5]
(global-set-key (kbd "M-1") 'shell-command);; shell command to M-1 instead of M-!
(global-set-key (kbd "C-,") 'comment-or-uncomment-region);; comment/uncomment region
(global-set-key "\M-\d" 'backwards-kill-word);; stop backspace from copying

(global-set-key (kbd "M-n") 'next-blank-line)
(global-set-key (kbd "M-p") 'previous-blank-line)
(define-key global-map [f8] 'replace-string)
(global-set-key "\C-c\C-k" 'copy-line)

;; ----- functions -----

(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))


(defun make-header ()
  "Format the give file as ao header file"
  (interactive)
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (insert "#IFNDEF ")
  (insert BaseFileName)
  (insert "_h_\n")
  (insert "#define ")
  (insert BaseFileName)
  (insert "_h_\n")
  (insert "\n")
  (insert "/* header definitions */")
  (insert "\n")
  (insert "#endif")
  )

(defun replace-string (FromString ToString)
  "Replace a string with another string."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
    ))
(setq max-lisp-eval-depth 10000)


;; experimental functions
(defun previous-blank-line ()
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
  )

(defun next-blank-line ()
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
  )


(defun switch-bg ()
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'switch-bg 'state)
      (progn
	(moe-dark)
	(put 'switch-bg 'state nil))
    (progn
      (moe-light)
      (put 'switch-bg 'state t))))

(defun my-display-buffer-below (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'below))
          (t
            (split-window (selected-window) nil 'below)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun my-display-buffer-above (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'above))
          (t
            (split-window (selected-window) nil 'above)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun my-display-buffer-left (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'left))
          (t
            (split-window (selected-window) nil 'left)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun my-display-buffer-right (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'right))
          (t
            (split-window (selected-window) nil 'right)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun dired-display-above ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-above buffer nil)))

(defun dired-display-below ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-below buffer nil)))

(defun dired-display-left ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-left buffer nil)))

(defun dired-display-right ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-right buffer nil)))

(defun ts-comm ()
  "Generate a comment"
  (interactive)
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (insert "/*\n")
  (insert (concat " * " (current-time-string) " Caleb Ellis\n"))
  (insert "* TODO: write comment\n")
  (insert "*/\n")
  )

(provide 'keybindings)
