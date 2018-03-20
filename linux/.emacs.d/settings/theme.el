;; theme
(require 'moe-theme)
(setq moe-theme-highlight-buffer-id nil)
(moe-dark)
(moe-theme-set-color 'cyan)

(require 'font-lock) ;; text
(global-hi-lock-mode 1)

;; fonts
(set-face-attribute 'default nil :font "Inconsolata-11")
;; text decoration
(require 'font-lock)
(global-hi-lock-mode nil)


;; tabs
(setq tab-width 8)
(setq-default indent-tabs-mode nil)

;; Bright-red TODOs
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

(provide 'theme)
