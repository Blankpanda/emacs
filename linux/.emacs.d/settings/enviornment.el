;; setup
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq undo-limit 2000000)
(setq undo-strong-limit 4000000)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-fringe-mode 0)

(split-window-horizontally) ;;split on startup
(display-time);; clock
(setq scroll-step 3);; Smooth scroll
(mouse-wheel-mode 1);; scroll with mousewheel
(global-hl-line-mode 0);; highlight line in use
(blink-cursor-mode 0);; stop cursor blink
(cua-mode 1) ;; normal copy/cut/past
(electric-pair-mode 1) ;; auto complete another parenthesis when one is typed
(global-visual-line-mode 1) ;; text wrapping
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)));; i-beam cursor
;; (setq pop-up-frames t);; new files open up in a seperate window
(global-linum-mode 1);; line numbers
(require 'paren);; highlight parenthesis
(show-paren-mode t)
(size-indication-mode 1);; size of file

;; make it so all emacs backusp go to a backups folder
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; ido mode
(load-library "view")
(require 'cc-mode)
(require 'ido)
(ido-mode t)

(provide 'enviornment)
