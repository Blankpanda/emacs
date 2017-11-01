; a lot of credit goes towards Casey Muratori for some of the functions in this file
(when (>= emacs-major-version 24)
 (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "https://melpa.org/packages/")
   t)
  (package-initialize))
;; settings folder
(add-to-list 'load-path "~/.emacs.d/settings")


;; setup
(setq initial-scratch-message "")
(setq inhibit-startup-screen 1)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-fringe-mode 0)

(split-window-horizontally) ;; split on startup
(setq scroll-step 3) ;;  Smooth scroll
(display-time) ;; Clock
(mouse-wheel-mode 1) ;; scroll with mousewheel
;;(global-hl-line-mode 1) ;; highlight line in use
(blink-cursor-mode 1) ;; stop cursor blink
(cua-mode 1) ;; normal copy/cut/paste
(electric-pair-mode 1) ;; auto complete another parenthesis when one is typed
(global-visual-line-mode 1) ;; text wrapping
(modify-all-frames-parameters (list (cons 'cursor-type 'bar))) ;; i-beam cursor
;; (setq pop-up-frames t) ;; new files open up in a seperate window
(global-linum-mode 0) ;; line numbers
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup"))) ;; make it so all emacs backusp go to a backups folder
(require 'paren) ;; highlight parenthesis
(show-paren-mode t)
(setq show-paren-style 'expression) ;; size indication of file
(size-indication-mode 1)

(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

;; ------ keybindings -------


(global-set-key [f1] 'dired) ;;run the dired command [f1]
(global-set-key [f4] 'goto-line) ;; f4 to goto line
(global-set-key [f5] 'compile) ;; run compile on f5
(global-set-key (kbd "M-1") 'shell-command) ;; shell command it M-1 instead of M-!
(global-set-key (kbd "C-,") 'comment-region) ;; comment/uncomment keys
(global-set-key (kbd "C-.") 'uncomment-region)
(global-set-key (kbd "M-n") 'next-blank-line)
(global-set-key (kbd "M-p") 'previous-blank-line)
(global-set-key "\C-c\C-k" 'copy-line)
(global-set-key (kbd "C-%") 'copy-line)
(global-set-key "\M-\d" 'backward-kill-word) ;; stop back space from copying
(define-key global-map [f8] 'replace-string)

;; yas
(setq yas-prompt-functions
        '(yas-dropdown-prompt
          yas-ido-prompt
          yas-x-prompt
          yas-completing-prompt
          yas-no-prompt))
(yas-global-mode 1))

;; easier windows movement (<ALT> + ARROW KEYS)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; code snippets
(require 'yasnippet)
(yas-global-mode 1 )

;; auto complete
(require 'auto-complete-settings)

;;theme
 (require 'moe-theme)
 (setq moe-theme-highlight-buffer-id nil)
 (moe-dark)
 (moe-theme-set-color 'orange)

;; text decoration
(require 'font-lock)
(global-hi-lock-mode nil)

;; fonts
(set-face-attribute 'default nil :font "Consolas-10")
;; 4-space tabs
(setq tab-width 4
      indent-tabs-mode nil)


; Bright-red TODOs
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode ))
 (make-face 'font-lock-fixme-face)
 (make-face 'font-lock-note-face)
 (mapc (lambda (mode)
	 (font-lock-add-keywords
	  mode
	  '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
	    ("\\#\\(TODO\\)" 1 'font-lock-fixme-face t))))
	fixme-modes)
 (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
 (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
 
;; languages

;; ---- languages ----

;; C/C++
(defconst petar-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
    "petar's Big Fun C++ Style")



;; ----- functions -----

(defun make-header ()
  "Format the give file as a header file"
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

(defun write-selected-text (name)
  "write mark to file in current buffers location."
  (write-region (buffer-substring region-beginning region-end) nil (append default-directory name) 'append)
)


(defun replace-string (FromString ToString)
  "Replace a string with another string."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
    ))
(setq max-lisp-eval-depth 10000)
(define-key global-map [f8] 'replace-string)

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


(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
