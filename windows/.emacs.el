(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; setup
(setq initial-scratch-message "")
(setq inhibit-startup-mnessage t)
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


;; comment/uncomment keys
(global-set-key (kbd "C-,") 'comment-region)
(global-set-key (kbd "C-.") 'uncomment-region)


;; easier windows movement (<ALT> + ARROW KEYS)
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
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(blink-cursor-mode nil)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   (quote
    ("3a69621a68c2d3550a4c777ffc000e1ea66f5bc2f61112814c591e1bda3f5704" "b573eefcc1061bb8b3aec52b6ef20d92e39bdba69711f8fa59da5b740c118666" "f3d428cce84446ab2bf96619fb199b5cb1b7231278aa11e51b2a627bff6cb973" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "1ccf1153f9ed823ddc01eff1bebe711e7c0ab4f09f64dbde3be0c3aff860a274" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



