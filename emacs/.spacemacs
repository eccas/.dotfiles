;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     csv
     javascript
     html
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     ;; auto-completion
     ;; better-defaults
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory nil)
     semantic
     better-defaults
     emacs-lisp
     git
     (python :variables
             python-enable-yapf-format-on-save t)
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t)
     syntax-checking
     (c-c++ :variables
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode)
     cscope
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t)
     markdown
     (shell :variables
            shell-default-shell 'ansi-term
            shell-default-height 30
            shell-default-position 'bottom)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(powerline)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     ;vi-tilde-fringe
     ;spaceline
     evil-search-highlight-persist
     )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         spacemacs-dark
                         spacemacs-light
                         ;solarized-light
                         ;solarized-dark
                         sanityinc-solarized-light
                         sanityinc-solarized-dark
                         zenburn
                         leuven
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("DejaVu Sans Mono for Powerline"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; Turn on flyspell for certain hooks
  (dolist (hook '(latex-mode-hook LaTeX-mode-hook))
    (add-hook hook (lambda () (flyspell-mode t))))

  ;; Always follow symlinks
  (setq vc-follow-symlinks t)

  ;; Set the default shell
  ;(setq shell-file-name "/bin/bash")

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; --------------- Key mappings ---------------
  (define-key evil-emacs-state-map (kbd "C-z") nil)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-v") 'scroll-up)
  (global-set-key (kbd "M-v") 'scroll-down)
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "M-Å") 'backward-paragraph)
  (global-set-key [\M-S-dead-circumflex] 'forward-paragraph)

  ;; Scroll without moving point
  (global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
  (global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))

  ;; Bind M-up and M-down to move-text-up and move-text-down
  (move-text-default-bindings)

  ;; --------------- Helm settings ---------------
  ;; Make helm behave more like ido
  (require 'helm)
  (defun fu/helm-find-files-navigate-back (orig-fun &rest args)
    (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
        (helm-find-files-up-one-level 1)
      (apply orig-fun args)))
  (advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)
  (defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
    (if (file-directory-p (helm-get-selection))
        (apply orig-fun args)
      (helm-maybe-exit-minibuffer)))
  (advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
  (define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)

  ;; Make helm preserve split window state
  (setq helm-always-two-windows nil)

  ;; Increase size of buffer name column in helm
  (setq helm-buffer-max-length 30)

  ;; Helm fuzzy matching
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  ;; Avoid annoying warning message about helm bookmark
  (require 'helm-bookmark)

  ;; --------------- Smartparens ---------------
  (require 'smartparens)
  (smartparens-global-mode t)
  (sp-local-pair 'c++-mode "<" ">" :actions '(wrap))
  (global-set-key (kbd "M-m j c") 'sp-rewrap-sexp)
  (global-set-key (kbd "M-m j C") 'sp-unwrap-sexp)

  ;; --------------- Pending Delete Mode ---------------
  (pending-delete-mode t)

  ;; --------------- Highlight parens ---------------
  (global-highlight-parentheses-mode t)

  ;; --------------- LaTeX ---------------
  ;; LaTeX full document preview
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  ;; Next error key-binding
  (add-hook 'LaTeX-mode-hook
            (lambda () (local-set-key (kbd "C-c e") 'TeX-next-error)))

  ;; --------------- Powerline  ---------------
  ;; Replace spaceline with powerline to get better scrolling performance
  ;; To bring spaceline back, remove it from dotspacemacs-excluded-packages
  ;(require 'powerline)
  ;(powerline-default-theme)

  ;; -------------------- Org --------------------
  (global-set-key (kbd "C-c o")
                  (lambda () (interactive) (find-file "~/notes.org")))
  (defun my-org-mode-config ()
    "For use in 'org-mode-hook'."
    (local-set-key (kbd "M-m m i i") 'org-insert-item)
    (org-defkey org-mode-map [(meta return)] 'org-meta-return)
    )
  (add-hook 'org-mode-hook 'my-org-mode-config)
  ; Can disable org-bullets-mode to show stars instead of UTF8 bullets. With
  ; org-hide-leading-stars set to nil that will show all stars.

  ;; -------------------- sr-speedbar --------------------
  ;; (require 'sr-speedbar)
  ;; (setq sr-speedbar-right-side nil)
  (setq speedbar-show-unknown-files t)
  (setq speedbar-use-images t)
  (setq speedbar-update-flag nil)
  ;; (setq sr-speedbar-skip-other-window-p t)
  ;; (setq sr-speedbar-auto-refresh nil)

  ;; ----------------- Programming --------------------
  ;; Set up company-clang completion.
  ;; Company-clang requires a .clang_complete file which defines all the
  ;; compilation flags, especially include directories and language standards.
  ;; It is possible to generate this file with the tool "cc_args.py". Download
  ;; this file, put it in e.g. "/usr/bin/cc_args.py", then run it like this:
  ;; 'make CXX="cc_args.py g++"'. The generated ".clang_complete" file should be
  ;; somewhere in the current folder or somewhere up the directory tree.
  ;; For clang to work, symlink the executable to /usr/bin/clang.
  ;; (defun indent-or-complete ()
  ;;   (interactive)
  ;;   (if (looking-at "\\_>")
  ;;       (company-complete-common-or-cycle)
  ;;     (indent-according-to-mode)))
  ;; (defun set-tab-key ()
  ;;   (local-set-key (kbd "TAB") 'indent-or-complete))
  ;; (define-key c-mode-base-map [tab] 'indent-or-complete)
  (setq company-async-timeout 10)

  ;; Problem with indentation of e.g. {} vs () and lambdas should be fixed with
  ;; CC Mode 5.33. However, installing it seems to be difficult.
  ;; (add-to-list 'load-path "/home/jsjunneb/.emacs.d/private/cc-mode-5.33")

  ;; Irony
  ;; (require 'irony)
  ;; (add-hook 'c++-mode-hook 'irony-mode)
  ;; (add-hook 'c-mode-hook 'irony-mode)
  ;; (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends '(company-irony))))

  ;; Flycheck
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

  ;; C style
  (defun followed-by (cases)
    (cond ((null cases) nil)
          ((assq (car cases)
                 (cdr (memq c-syntactic-element c-syntactic-context))) t)
          (t (followed-by (cdr cases)))))
  (c-add-style "personal"
               '("mathworks-electric"
                 (c-offsets-alist
                  (innamespace . 0)
                  (innamespace
                   . (lambda (x)
                       (if (followed-by
                            '(innamespace namespace-close)) 0 '+)))
                  ;; (innamespace
                  ;;  . (lambda (x)
                  ;;      (if (followed-by
                  ;;           '(innamespace namespace-open)) '- '+)))
                  (case-label . *)
                  (access-label . *)
                  (label . *)
                  (statement-cont c-lineup-cascaded-calls) ; default: sb-c11hack-align-enum-class-closing-brace
                  ;; (statement-cont sb-c11hack-align-enum-class-closing-brace) ; default: c-lineup-cascaded-calls
                  )))
  (setq c-default-style "personal")
  ;; (setq-default c-default-style "k&r")
  ;; (setq-default c-basic-offset 4)
  ;; (setq-default indent-tabs-mode nil)

  ;; Objective-C settings
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

  ;; Semantic Settings
  ;; Turn on semantic debugger
  ;; (require 'semantic/analyze/debug)

  ;; Turn off function definition shown in status line
  (add-hook 'semantic-mode-hook (lambda () (semantic-idle-summary-mode -1)))

  ;; Set semantic search order
  (add-hook 'semantic-mode-hook (lambda()
                                  (setq-mode-local c++-mode
                                                   semanticdb-find-default-throttle
                                                   '(file local project unloaded system recursive))))
  ;; -------------------- Misc --------------------
  ;; Function to sort lines case insensitive
  (defun sort-lines-nocase ()
    (interactive)
    (let ((sort-fold-case t))
      (call-interactively 'sort-lines)))

  ;; Do not show trailing whitespace
  (setq spacemacs-show-trailing-whitespace nil)
  (setq show-trailing-whitespace nil)

  ;; Delete trailing whitespace on save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Turn off persistent highlight of searches
  (setq global-evil-search-highlight-persist -1)

  ;; Make sure that lisp state is not entered after e.g. sexp slurp
  (setq evil-lisp-state-enter-lisp-state-on-command nil)

  ;; Always use spaces instead of tabs
  (setq-default indent-tabs-mode nil)
  (defun my-makefile-mode-hook ()
    (setq indent-tabs-mode t)
    (setq tab-width 4))
  (add-hook 'makefile-mode-hook 'my-makefile-mode-hook)
  (add-hook 'nxml-mode-hook (lambda () (setq tab-width 3)))
  (setq-default nxml-child-indent 3)
  (setq-default nxml-attribute-indent 3)

  ;; Turn on camel case motion globally
  (spacemacs/toggle-camel-case-motion-globally)

  ;; For clang to work, symlink the executable to /usr/bin/clang.
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-window-height 10)
 '(gdb-non-stop-setting nil t)
 '(git-commit-summary-max-length 100)
 '(message-fill-column 100)
 '(monokai-highlight-line "#2B2C26")
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars nil)
 '(org-list-indent-offset 0)
 '(package-selected-packages
   (quote
    (yapfify xterm-color web-mode unfill tagedit stickyfunc-enhance srefactor smeargle slim-mode shell-pop scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements ox-twbs ox-gfm orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mwim multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit-popup live-py-mode hy-mode htmlize helm-pydoc helm-gitignore helm-css-scss helm-cscope xcscope helm-company helm-c-yasnippet haml-mode gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip flycheck evil-magit magit transient git-commit with-editor eshell-z eshell-prompt-extras esh-help emmet-mode disaster cython-mode csv-mode company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-c-headers company-auctex company-anaconda company cmake-mode clang-format auto-yasnippet auto-dictionary auctex anaconda-mode pythonic ac-ispell auto-complete monokai-theme ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js2-refactor js-doc indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode coffee-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F8F8F2" :background "#000000"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(helm-ff-file ((((class color) (min-colors 257)) (:foreground "#F8F8F2" :background "#272822")) (((class color) (min-colors 89)) (:foreground "#F8F8F2" :background "#000000"))))
 '(isearch ((t (:inherit region :background "#679A01" :foreground "#F8F8F2"))))
 '(lazy-highlight ((t (:inherit highlight :background "#75715E" :foreground "#F8F8F2"))))
 '(org-code ((t (:foreground "#F92672"))))
 '(sp-show-pair-match-face ((t (:background "#272822" :foreground "#679A01" :weight bold :inverse-video t))))
 '(sp-show-pair-mismatch-face ((t (:background "#272822" :foreground "#F20055" :weight bold :inverse-video t))))
 '(woman-unknown ((t (:inherit font-lock-warning-eface :foreground "red4")))))
