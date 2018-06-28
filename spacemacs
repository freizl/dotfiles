;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; start spacemacs separately with other emacs instance
;;;;; HOME=~/spacemacs /Applications/Emacs.app/Contents/MacOS/Emacs

(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

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
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; version-control
     ;; typescript
     auto-completion
     better-defaults
     csv
     elm
     emacs-lisp
     emoji
     git
     helm
     html
     java
     (javascript :variables
                 javascript-disable-tern-port-files nil
                 node-add-modules-path t
                 )
     markdown
     org
     plantuml
     search-engine
     shell-scripts
     spell-checking
     sql
     syntax-checking
     xkcd
     yaml
     (haskell :variables
              haskell-enable-hindent-style "johan-tibell")
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(plantuml-mode)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
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
   dotspacemacs-editing-style 'vim
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
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 9))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1)
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
   dotspacemacs-persistent-server t
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
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
)

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (setq auto-save-interval 100
        auto-save-timeout 15
        appt-display-duration 20
        blink-cursor-mode nil
        browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        column-number-mode t
        size-indication-mode t
        erc-nick "haisheng"
        erc-notifications-mode t
        size-indication-mode t
        display-time-mode t)

  (setenv "GRAPHVIZ_DOT" "/opt/local/bin/dot")
  (spacemacs/set-leader-keys "oct" 'org-todo)
  (setq spaceline-org-clock-p t)
  (setq org-html-doctype "html5")
  (setq org-agenda-include-diary t)
  (setq org-todo-keywords
      '((sequence "TODO(t)" "BLOCK(b)" "INVESTIGATING(v)" "INPROGRESS(i)" "CODEREVIEW(r)" "MERGING(m)" "DONE(d)")
        (sequence "|" "CANCELED(c)")))
  (setq org-todo-keyword-faces
      '(("BLOCK" . (:foreground "#E34843" :weight bold))
        ("INVESTIGATING" . (:foreground "#F1D10D"))
        ("INPROGRESS" . (:foreground "#007DC1" :weight bold))
        ("CODEREVIEW" . (:foreground "#A9E6FD"))
        ("MERGING" . (:foreground "#46B090"))
        ("CANCELED" . (:foreground "#A7A7A7" ))
        ))
  (setq org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (haskell . t)
     (plantuml . t)
     (css . t)
     (awk . t)
     (python . t)
     (perl . t)
     (clojure . t)
     (js . t)
     (dot . t)
     (ditaa . t)
     (sh . t)
     (makefile . t))))
  ;; (setq org-agenda-files '("/path/to/file.org"))

  (setq org-use-speed-commands t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar" (getenv "HOME")))
  (setq projectile-enable-caching t)
  (setq projectile-project-root-files-functions
   (quote
    (projectile-root-top-down projectile-root-bottom-up projectile-root-top-down-recurring)))

  (setq-default js2-basic-offset 2)
  (setq-default js-indent-level 2)

  (setq magit-auto-revert-mode nil
        magit-branch-arguments (quote (""))
        magit-push-always-verify nil)

  (setq eclim-eclipse-dirs '("~/../eclipse/java-oxygen/Eclipse.app/Contents/Eclipse")
        eclimd-default-workspace "~/../eclipse-workspace"
        eclim-executable "~/../eclipse/java-oxygen/Eclipse.app/Contents/Eclipse/eclimd")

  (setq css-indent-offset 2)
  (load-file "./.okta.emacs")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (add-node-modules-path xterm-color unfill shell-pop mwim multi-term skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors js2-mode helm-c-yasnippet flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck-elm eshell-z eshell-prompt-extras esh-help emoji-cheat-sheet-plus company-emoji auto-yasnippet auto-dictionary packed tide typescript-mode plantuml-mode csv-mode sql-indent yaml-mode xkcd web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode intero flycheck insert-shebang hlint-refactor hindent helm-hoogle helm-css-scss helm-company haskell-snippets haml-mode fuzzy fish-mode engine-mode emmet-mode elm-mode company-web web-completion-data company-tern dash-functional company-statistics company-shell company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company cmm-mode ac-ispell auto-complete org-mime smeargle orgit org-category-capture org-present org-pomodoro alert log4e gntp org-download markdown-toc magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub with-editor mmm-mode markdown-mode gh-md tern ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js2-refactor js-doc info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode coffee-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets symon string-inflection spaceline-all-the-icons all-the-icons memoize powerline password-generator spinner overseer org-projectile org-brain nameless mvn meghanada maven-test-mode magit-svn json-navigator hierarchy impatient-mode parent-mode helm-xref helm-purpose window-purpose imenu-list groovy-mode groovy-imports pcache gradle-mode flycheck-bashate flx evil-org let-alist evil-lion iedit evil-goggles evil-cleverparens smartparens paredit anzu highlight ensime sbt-mode scala-mode emojify ht editorconfig dante lcr f counsel-projectile projectile counsel swiper ivy pkg-info epl s dash centered-cursor-mode yasnippet helm avy helm-core popup hydra font-lock+ evil goto-chg undo-tree bind-map bind-key async add-node-modules-path xterm-color unfill shell-pop mwim multi-term skewer-mode simple-httpd json-snatcher json-reformat multiple-cursors js2-mode helm-c-yasnippet flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck-elm eshell-z eshell-prompt-extras esh-help emoji-cheat-sheet-plus company-emoji auto-yasnippet auto-dictionary packed tide typescript-mode plantuml-mode csv-mode sql-indent yaml-mode xkcd web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode intero flycheck insert-shebang hlint-refactor hindent helm-hoogle helm-css-scss helm-company haskell-snippets haml-mode fuzzy fish-mode engine-mode emmet-mode elm-mode company-web web-completion-data company-tern dash-functional company-statistics company-shell company-ghci company-ghc ghc haskell-mode company-emacs-eclim eclim company-cabal company cmm-mode ac-ispell auto-complete org-mime smeargle orgit org-category-capture org-present org-pomodoro alert log4e gntp org-download markdown-toc magit-gitflow htmlize helm-gitignore gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link evil-magit magit magit-popup git-commit ghub with-editor mmm-mode markdown-mode gh-md tern ws-butler winum which-key web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum livid-mode linum-relative link-hint json-mode js2-refactor js-doc info+ indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish define-word column-enforce-mode coffee-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
