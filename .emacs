;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;;; Required packages
;;; everytime emacs starts, it will automatically check if those packages are
;;; missing, it will install them automatically
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar tmtxt/packages
  '(irony company-irony company yasnippet google-c-style flycheck-google-cpplint company-c-headers autopair flycheck))
(dolist (p tmtxt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; C/C++ setup
(add-hook 'c++-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'column-number-mode)
(add-hook 'c++-mode-hook 'show-paren-mode)

;; Irony
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (optional) bind TAB for indent-or-complete
(defun irony--check-expansion ()
(save-excursion
  (if (looking-at "\\_>") t
    (backward-char 1)
    (if (looking-at "\\.") t
      (backward-char 1)
      (if (looking-at "->") t nil)))))
(defun irony--indent-or-complete ()
"Indent or Complete"
(interactive)
(cond ((and (not (use-region-p))
            (irony--check-expansion))
       (message "complete")
       (company-complete-common))
      (t
       (message "indent")
       (call-interactively 'c-indent-line-or-region))))
(defun irony-mode-keys ()
"Modify keymaps used by `irony-mode'."
(local-set-key (kbd "TAB") 'irony--indent-or-complete)
(local-set-key [tab] 'irony--indent-or-complete))
(add-hook 'c-mode-common-hook 'irony-mode-keys)

;; Add company-irony to your company backends.
(require 'company)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)

;; company-c-headers (#include <header> completions)
(add-to-list 'company-backends 'company-c-headers)
(customize-set-variable 'company-c-headers-path-system 
			(quote ("/usr/include/" 
				"/usr/local/include/" 
				"/usr/include/c++/4.8/" 
				"/usr/include/x86_64-linux-gnu/c++/4.8/" 
				"/usr/include/c++/4.8/backward" 
				"/usr/lib/gcc/x86_64-linux-gnu/4.8/include" 
				"/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed" 
				"/usr/include/x86_64-linux-gnu")))

;; Flycheck
(require 'flycheck-irony)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
;(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-standard-library "libc++")))
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(setq irony-additional-clang-options (quote ("-std=c++11")))

;; Yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1) 
;; replace (yas-global-mode 1) with the next 3 lines to use as minor mode
;(yas-reload-all)
;(add-hook 'c++-mode-hook #'yas-minor-mode)
;(add-hook 'c-mode-hook #'yas-minor-mode)

;; flycheck-google-cpplint 
(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     (flycheck-add-next-checker 'irony
                                'c/c++-googlelint 'append)))
(customize-set-variable 'flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
(customize-set-variable 'flycheck-googlelint-filter "-legal/copyright")

;; google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; TeX/LaTeX setup
(require 'tex)
(setq TeX-PDF-mode t)
(TeX-global-PDF-mode t)
(customize-set-variable 'TeX-PDF-mode t)

;; Java setup
(add-hook 'java-mode-hook 'set-newline-and-indent)
(add-hook 'java-mode-hook 'show-paren-mode 1)
(add-hook 'java-mode-hook 'column-number-mode)

;; nyan cat!
;;(add-hook 'prog-mode-hook #'nyan-mode)
;(nyan-mode)

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun unindent-region ()
    (interactive)
    (indent-region (region-beginning) (region-end) -1))

(global-set-key (kbd "C-c C-v") 'uncomment-region)
(global-set-key [backtab] 'unindent-region)
(global-set-key (kbd "C-z") 'undo)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'load-path "~/.emacs.d/")

(require 'iso-transl)

;; Themes
(load-theme 'leuven t)
(defun set-light-theme ()
  (interactive)
  "Set light theme"
  (disable-theme 'leuven)
  (disable-theme 'tango-dark)
  (load-theme 'leuven t))
(defun set-dark-theme ()
  (interactive)
  "Set dark theme"
  (disable-theme 'tango-dark)
  (disable-theme 'leuven)
  (load-theme 'tango-dark t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))

;; Ignore everything below this
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(company-c-headers-path-system (quote ("/usr/include/" "/usr/local/include/" "/usr/include/c++/4.8/" "/usr/include/x86_64-linux-gnu/c++/4.8/" "/usr/include/c++/4.8/backward" "/usr/lib/gcc/x86_64-linux-gnu/4.8/include" "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed" "/usr/include/x86_64-linux-gnu")))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes (quote ("47689e72383079f1278ddf0e3fe82053ff59a75ea0575c68837453a12c1d2395" "65d0d6f943ac67c7d6f666b680bcb4ce75250a89d15be7554c7f7d03796061a8" "49af976294c20af4f1f0c40f6f2d98eaa4dbe39f925ad2ddb5aa6ee9398f5b3a" "716d52b8ab1fafb3d189fd2e0401d9fc5fac66030db95529752dd49e7020e152" "dbc20a9c4678ab497fe63a1859e245d3064855b7b3646fa7173a3c09ef47f991" "d583dc6aeab1f6ecbbb39ae8309d75708bfaab96cd88ff90807ad54f6cb570db" "e9b5bf54cfaa9d6bb67e60a0f98917f9979182a14728d31a6367763b48a96d3f" "0126b63b62634a1bf279c79bdcade6ce840c035acfda93c4b4600c269feec09e" "e51deb697da5333a29dc99fbfaaeaeae3dfa4a45a1df6f3d1ec5d654d73ba960" "b5a1846883fda0198f3c64d9e4ea123be00b16a943b594e26243552615f066cb" "b7af568972ba4d93b5f22c7f57f473dc74b37e38d8728b55bf6751a805aaab1a" "87b7217dff1a429e7542da999cb495bcf200494d8e6d8fb244018d66f8368e7b" "d6729a519fdad58712d8dc799770c0da88d9f3350a651ec5222f55c661570089" "0f059117f31676bd092297fe0aaac0e27aa540972642d6f9e0cefe1a456dc09f" "c9581754751fe3e192286d059ec7747971716660c36ccb8031d6adc0cd09749b" "a7079fae00f0a0e72b83667fb32f17d73ceb5cbdeb8b566b7b8101b6a16db64c" "5dcc1fe0183d3b347dde502d2cd1264232dc974294e1967b337122086c1dfd3d" "f0a6acc020dbe4665a16262bac299882ea6c5808ae3e1efeafd762e8e14771d5" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "3b24f986084001ae46aa29ca791d2bc7f005c5c939646d2b800143526ab4d323" "9cb6358979981949d1ae9da907a5d38fb6cde1776e8956a1db150925f2dad6c1" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "c0dd5017b9f1928f1f337110c2da10a20f76da0a5b14bb1fec0f243c4eb224d4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "83e584d74b0faea99a414a06dae12f11cd3176fdd4eba6674422539951bcfaa8" "eafda598b275a9d68cc1fbe1689925f503cab719ee16be23b10a9f2cc5872069" "7dd0db710296c4cec57c39068bfffa63861bf919fb6be1971012ca42346a417f" "65320d86c52e9019347ed725f2a7c07705be5acb38bc83b92064e2489f6c3edc" "685a7460fdc4b8c38796234d3a96b3aacbe4fba739fb33b5d6d149051ce74a58" default)))
 '(delete-selection-mode 1)
 '(fci-rule-color "#eee8d5")
 '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
 '(flycheck-googlelint-filter "-legal/copyright")
 '(font-lock-maximum-decoration (quote ((t . t))))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#fdf6e3" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors (quote (("#eee8d5" . 0) ("#B4C342" . 20) ("#69CABF" . 30) ("#69B7F0" . 50) ("#DEB542" . 60) ("#F2804F" . 70) ("#F771AC" . 85) ("#eee8d5" . 100))))
 '(hl-bg-colors (quote ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors (quote ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(rainbow-identifiers-cie-l*a*b*-lightness 30)
 '(rainbow-identifiers-cie-l*a*b*-saturation 35)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#c85d17") (60 . "#be730b") (80 . "#b58900") (100 . "#a58e00") (120 . "#9d9100") (140 . "#959300") (160 . "#8d9600") (180 . "#859900") (200 . "#669b32") (220 . "#579d4c") (240 . "#489e65") (260 . "#399f7e") (280 . "#2aa198") (300 . "#2898af") (320 . "#2793ba") (340 . "#268fc6") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (quote (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
