(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; setup
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode 0)
(menu-bar-mode 1 )
(tool-bar-mode 0)

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
(global-linum-mode 1)
;; make it so all emacs backusp go to a backups folder
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; easier windows movement (<SHIFT> + ARROW KEYS)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; code snippets
(require 'yasnippet)
(yas-global-mode 1 )

;; java
(require 'jdee)

;; theme
(require 'moe-theme)
(setq moe-theme-highlight-buffer-id t)
(moe-dark)
(moe-theme-set-color 'green)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(cua-mode t nil (cua-base))
 '(tool-bar-mode nil))
