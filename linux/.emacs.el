(when (>= emacs-major-version 24)
(require 'package)
(add-to-list
  'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(add-to-list 'load-path "~/.emacs.d/settings") ;; settings folder
(add-to-list 'load-path "~/.emacs.d/languages") ;; languages

(require 'enviornment) ;; settings/enviornment.el
(require 'editing) ;; settings/editing.el
(require 'org) ;; settings/org.el
(require 'keybindings) ;; settings/keybindings.el
(require 'theme) ;; settings/theme.el
(require 'auto-complete-settings) ;; settings/auto-complete-settings.el
(require 'rainbow-mode-settings) ;; settings/rainbow-mode-settings.el
(require 'web-mode-settings) ;; settings/web-mode-settings.el

(require 'java) ;; languages/java.el
(require 'cpp)  ;; languages/cpp.el
(require 'python);;languages/python.el
(require 'elisp) ;;languages/elisp.el


;; TODO (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
