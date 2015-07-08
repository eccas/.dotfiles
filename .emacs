(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(delete-selection-mode 1)
 '(font-lock-maximum-decoration (quote ((t . t))))
 '(inhibit-startup-screen t))

(setq TeX-PDF-mode t)
;(setq TeX-auto-save t)
;(setq TeX-parse-self t)
;(setq TeX-save-query nil)

(require 'tex)
(TeX-global-PDF-mode t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun unindent-region ()
    (interactive)
    (indent-region (region-beginning) (region-end) -1))

(add-hook 'java-mode-hook 'set-newline-and-indent)
(add-hook 'java-mode-hook 'show-paren-mode 1)
(add-hook 'java-mode-hook 'column-number-mode)
(add-hook 'c++-mode-hook 'set-newline-and-indent)
(add-hook 'c++-mode-hook 'column-number-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode) ;Pairs parantheses, brackets
(setq-default c-basic-offset 4
	      indent-tabs-mode nil)
(global-set-key [backtab] 'unindent-region)
(global-set-key (kbd "C-z") 'undo)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'load-path "~/.emacs.d/")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;(ac-config-default)

(require 'iso-transl)

;;;; --Setup JDEE--
;; (add-to-list 'load-path (expand-file-name "~/emacs/site/jdee-2.4.0.1/lisp"))
;; (add-to-list 'load-path (expand-file-name "~/emacs/site/cedet-1.1/common"))
;; (add-to-list 'load-path (expand-file-name "~/emacs/site/elib-1.0"))
;; (load-file (expand-file-name "~/emacs/site/cedet-1.1/common/cedet.el"))
;; (setq defer-loading-jde t)
;; (if defer-loading-jde
;;     (progn
;;       (autoload 'jde-mode "jde" "JDE mode." t)
;;       (setq auto-mode-alist
;; 	    (append
;; 	     '(("\\.java\\'" . jde-mode))
;; 	     auto-mode-alist)))
;;   (require 'jde))
;; (defun my-jde-mode-hook ()
;;   (setq c-basic-offset 2))
;; (add-hook 'jde-mode-hook 'my-jde-mode-hook)
;; (define-obsolete-function-alias 'make-local-hook 'ignore "21.1")
;;;;
