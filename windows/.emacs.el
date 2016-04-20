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
(scroll-bar-mode 0)
(menu-bar-mode 1)
(tool-bar-mode 0)
(set-fringe-mode 0)


;; scroll with mousewheel
(mouse-wheel-mode 1)
;; highlight line in use
(global-hl-line-mode 1)
;; stop cursor blink
(blink-cursor-mode 0)
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
(show-paren-mode 1)
;; size indication of file
(size-indication-mode 1)

;; ------ keybindings -------

;; f4 to goto line
(global-set-key [f4] 'goto-line) 

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
;; java
(require 'jdee)

;;theme
(require 'moe-theme)
(setq moe-theme-highlight-buffer-id t)
(moe-dark)
(moe-theme-set-color 'cyan)
;;(load-theme 'monokai t)

;; text decoration
(require 'font-lock)
(global-hi-lock-mode nil)

;; fonts
(set-face-attribute 'default nil :font "Consolas-10")

