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
(fringe-mode 0)
;;split on startup
(split-window-horizontally)
; Smooth scroll
(setq scroll-step 3)
; Clock
(display-time)
;; scroll with mousewheel
(mouse-wheel-mode 1)
;; highlight line in use
;(global-hl-line-mode 1)
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

(setq font-lock-maximum-decoration t)
(load-library "view")
(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

;; ------ keybindings -------
;;(global-set-key (kbd "TAB") 'tab-to-tab-stop)

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

(global-set-key "\C-c\C-k" 'copy-line)

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-%") 'copy-line)

;; stop back space from copying
(global-set-key "\M-\d" 'backward-kill-word)

;; easier windows movement (<ALT> + ARROW KEYS)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; code snippets
(require 'yasnippet)
(yas-global-mode 1 )

;; auto complete
(require 'auto-complete-settings)

;; multiline edit
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;theme

(require 'moe-theme)
(setq moe-theme-highlight-buffer-id nil)
(moe-dark)
(moe-theme-set-color 'cyan)
;;(load-theme 'subatomic t)
;;(load-theme 'monokai t)

;; text decoration
(require 'font-lock)
(global-hi-lock-mode nil)

;; fonts
(set-face-attribute 'default nil :font "Consolas-10")
;; 4-space tabs
(setq tab-width 4)
;      indent-tabs-mode nil)

; Bright-red TODOs
 (setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode js-mode web-mode))
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

;(defface jquery-sign '((t (:weight bold))) "Cyan")

;(font-lock-add-keywords 'js-mode '(("$" 0 'jquery-sign t)))
;; languages

;; ---- languages ----
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;; Web
(setq web-mode-engines-alist
      '(("php"    . "\\.pho\\'")
        ("blade"  . "\\.blade\\."))
)
; css
(add-hook 'css-mode-hook 'rainbow-mode)
					; json
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
;; php
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
;; html
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

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


(define-key global-map [f8] 'replace-string)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#28211c" "#cf6a4c" "#54be0d" "#f9ee98" "#5ea6ea" "#9b859d" "#5ea6ea" "#8a8986"])
 '(ansi-term-color-vector
   [unspecified "#28211c" "#cf6a4c" "#54be0d" "#f9ee98" "#5ea6ea" "#9b859d" "#5ea6ea" "#8a8986"])
 '(custom-safe-themes
   (quote
    ("e5fc3d797aea14743c42cdbddfede0c0b24362875fdafbf6127554007cbe3717" "6952b5d43bbd4f1c6727ff61bc9bf5677d385e101433b78ada9c3f0e3787af06" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "3b0a350918ee819dca209cec62d867678d7dac74f6195f5e3799aa206358a983" "cd0d4fdf764f757fd659ee2697239a62f38d15203000ced1ad8e43c978942c68" "ff3ca9d675ad57854fb5485831a3c7d5f34aa47af4de0c24335b7efa02dbb8f9" "264b639ee1d01cd81f6ab49a63b6354d902c7f7ed17ecf6e8c2bd5eb6d8ca09c" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "deccaabd69feab7f165156e5c4701c75df0ed386d05f4ba459fa71b71c26ee86" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "527df6ab42b54d2e5f4eec8b091bd79b2fa9a1da38f5addd297d1c91aa19b616" "5228973368d5a1ac0cbea0564d0cd724937f52cc06a8fd81fc65a4fa72ff837b" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "4cbec5d41c8ca9742e7c31cc13d8d4d5a18bd3a0961c18eb56d69972bbcf3071" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "85d609b07346d3220e7da1e0b87f66d11b2eeddad945cac775e80d2c1adb0066" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#000000" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
			 (:color "#c0c0c0"))
     (implicitParams :underline
		     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000"))))
 '(fci-rule-color "#383838" t)
 '(omnisharp-auto-complete-popup-help-delay nil)
 '(omnisharp-company-begin-after-member-access t)
 '(package-selected-packages
   (quote
    (railscasts-reloaded-theme railscasts-theme intellij-theme tangotango-theme darktooth-theme minimal-theme dired-single dired+ flycheck omnisharp yasnippet web-mode rainbow-mode multiple-cursors moe-theme json-mode cyberpunk-theme base16-theme auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
