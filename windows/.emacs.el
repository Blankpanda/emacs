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
(show-paren-mode t)
(setq show-paren-style 'expression)
;; size indication of file
(size-indication-mode 1)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#5f5f5f" "#ff4b4b" "#a1db00" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#ffffff"])
 '(custom-safe-themes
   (quote
    ("14aa6c39853d8bab802750a7451fd652f1000e44511cc8216fc95c008e6f71ad" "93004ad952e1d180ef6bd89f3adcb0061dd29c1b63afbebb1325f082b3bd7a36" "a53fc4c86b48e0501f738e0c7f9f42a04029cfbefa26d5d588fe01d48af4d180" "19ae9e7a29410830ef2f4b778bf911374daf8838c578f4768827d26fc2956fb7" "41d0400b0d0e6b438707c9978e6e5e0a5c7abb9dfbd879defb041a78f0c746be" "28111c756a45fc7f346fd1830c0d1cba0cd1178d3c7080405705eb836dca4c16" "fd09e842bb1336dd3d7ec748e297eb90bd5e0165757d3fbc1c14ad47f1c467df" "0323cc2d54bcd8944bc49329eacc16fac73b0cc124e7b7f0a486845b9ab052f1" "9aa56a924d0f6e3ae4bc505acab1620b0e54dc7019ca3cd7ba77d5420cf1e036" "a4210e3ca7b71d304f46c504f97b475b05b8fd09707bb5402b049a108ff56fb6" "c57aa5f299ca66ff103714850775196f1dcd962c74b3d74c6631de68c79b8d0a" "ce92af99cd38ceb2cd25b13f00df06c98d979796644e2491e40e5520550cd43e" "239e94b07e580edd7981381d68a15bfa2de20d5c40da7c9e6214b8ac8f72fc44" "71bba6b310fc126899baf0f7d51ae2377829c143e4e9ecdfa792e84767e6ee19" "085c437f703a58bc033464459e316540e0a26c86248767e5b7240ad0d10bde97" "8814d882389a83f10da0301da4abb0705ee7c946beafbd50d3d9aaaf0523d69c" "b573eefcc1061bb8b3aec52b6ef20d92e39bdba69711f8fa59da5b740c118666" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
