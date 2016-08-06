;;; Separate custom file
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

;;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Hide toolbar
(tool-bar-mode -1)

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
  '(irony company-irony company yasnippet google-c-style flycheck-google-cpplint company-c-headers autopair flycheck ggtags sr-speedbar))
(dolist (p tmtxt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; IDO
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

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
(setq company-clang-executable "/usr/bin/clang++-3.5")

;; company-c-headers (#include <header> completions)
(add-to-list 'company-backends 'company-c-headers)
(setq company-c-headers-path-system 
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
(setq flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
(setq flycheck-googlelint-filter "-legal/copyright,-build/header_guard,-build/c++11,-whitespace,-runtime/references,-build/include,-readability/todo,-readability/braces")

;; google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; GGTags
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
(setq-local imenu-create-index-function #'ggtags-build-imenu-index)(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; SR-Speedbar
(require 'sr-speedbar)
(setq sr-speedbar-skip-other-window-p t)

;; TeX/LaTeX setup
;; (require 'tex)
;; (setq TeX-PDF-mode t)
;; (TeX-global-PDF-mode t)
;; (setq TeX-PDF-mode t)

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

;; Enable upcase and downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
