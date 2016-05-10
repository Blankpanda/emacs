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
(setq inhibit-startup-message t)
(setq visible-bell t)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)
(fringe-mode 4)

;; scroll with mousewheel
(mouse-wheel-mode 1)
;; highlight line in use
(global-hl-line-mode 1)
;; stop cursor blink
(blink-cursor-mode 0)
 ;; normal copy/cut/past
(cua-mode 1)
 ;; auto complete another parenthesis when one is typed
(electric-pair-mode 1)
 ;; text wrapping
(global-visual-line-mode 1)
;; i-beam cursor
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
;; new files open up in a seperate window
;; (setq pop-up-frames t)
;; NO line numbers
(global-linum-mode 0)
;; highlight parenthesis
(require 'paren)
(show-paren-mode 1)
;; size of file
(size-indication-mode 1)

;; make it so all emacs backusp go to a backups folder
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

;; easier windows movement (<SHIFT> + ARROW KEYS)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))


;; ---- keybindings ----

;; run the dired command [f1]
(global-set-key [f1] 'dired)
;; goto line [f4]
(global-set-key [f4] 'goto-line)
;; run compile [f5]
(global-set-key [f5] 'compile)
;; shell command to M-1 instead of M-!
(global-set-key (kbd "M-1") 'shell-command)

;; comment/uncomment region
(global-set-key (kbd "C-.") 'comment-or-uncomment-region)


;; code snippets
(require 'yasnippet)
(yas-global-mode 1 )

;; auto complete mode
(require 'auto-complete-settings)
;; java
(require 'jdee)
(setq jde-jalopy-option-command-line-args "-lWARN")

;;python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)



;; theme
(load-theme 'grandshell t)

;; text
(require 'font-lock)
(global-hi-lock-mode nil)

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
    ("6c62b1cd715d26eb5aa53843ed9a54fc2b0d7c5e0f5118d4efafa13d7715c56e" "f81933744f47a010213537575f84085af3937b27748b4f5c9249c5e100856fc5" default)))
 '(org-agenda-files (quote ("~/shub.org")))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant oblique :weight bold :height 113 :width normal)))))

