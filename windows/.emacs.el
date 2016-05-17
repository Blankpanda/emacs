;; a lot of credit goes towards Casey Muratori for some of the functions in this file

(when (>= emacs-major-version 24)
 (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
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

;;split on startup
(split-window-horizontally)
; Smooth scroll
(setq scroll-step 3)
; Clock
(display-time)
;; scroll with mousewheel
(mouse-wheel-mode 1)
;; highlight line in use
(global-hl-line-mode 1)
(set-face-background 'hl-line "red")
(set-face-background 'region "red")
;; stop cursor blink
(blink-cursor-mode 0)
(set-cursor-color "red")
 ;; normal copy/cut/paste
(cua-mode 1)
 ;; auto complete another parenthesis when one is typed
(electric-pair-mode 1)
 ;; text wrapping
(global-visual-line-mode 1)
;; i-beam cursor
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
;; new files open up in a seperate window
;; (setq pop-up-frames t)
;; line numbers
(global-linum-mode 0)
;; make it so all emacs backusp go to a backups folder
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
;; highlight parenthesis
(require 'paren)
(show-paren-mode t)
(setq show-paren-style 'expression)
;; size indication of file
(size-indication-mode 1)


(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

;; ------ keybindings -------

;;run the dired command [f1]
(global-set-key [f1] 'dired)
;; f4 to goto line
(global-set-key [f4] 'goto-line)
;; run compile on f5
(global-set-key [f5] 'compile)

;; shell command it M-1 instead of M-!
(global-set-key (kbd "M-1") 'shell-command)
;; comment/uncomment keys
(global-set-key (kbd "C-,") 'comment-region)
(global-set-key (kbd "C-.") 'uncomment-region)


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
(moe-theme-set-color 'cyan)
;;(load-theme 'monokai t)

;; text decoration
(require 'font-lock)
(global-hi-lock-mode nil)

;; fonts
(set-face-attribute 'default nil :font "Consolas-10")
;; 4-space tabs
(setq tab-width 4
      indent-tabs-mode nil)


; Bright-red TODOs
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode))
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

(global-set-key (kbd "M-n") 'next-blank-line)
(global-set-key (kbd "M-p") 'previous-blank-line)
