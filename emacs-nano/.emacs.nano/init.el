(provide 'init)

(add-to-list 'load-path "~/.emacs.nano/lisp")

(defun icot/package-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))

;; From https://github.com/daviwil/emacs-from-scratch/blob/master/init.el
(defun icot/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'icot/display-startup-time)

;;; Basics: Native compilation
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(setq load-prefer-newer t)

;;; Native modes
;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; Store customizations in separate file
(setq custom-file "~/.emacs.nano/custom.el")
(load custom-file)

;; Create and configure auto-save folder and backups
(let ((savedir (concat user-emacs-directory "auto-save"))
      (backupdir (concat user-emacs-directory "backup")))
  (unless (file-exists-p savedir) (make-directory savedir))
  (unless (file-exists-p backupdir) (make-directory backupdir))
  (setq auto-save-file-name-transforms `((".*" ,savedir t)))
  (setq backup-directory-alist `(("." . ,backupdir))))

;; Auto-revert mode
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

;; Add Repositories
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

; ensure use-package installs all packages without requiring :straight t (ex: (use-package evil :straight t))
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

;;;; Auto-compile
(setq load-prefer-newer t)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Benchmark init

;(load "wrong-args-fix.el")

(use-package benchmark-init
 :ensure f
 :config
;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Theme, GUI

;; Disable menu bar and scroll bar
(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

(global-prettify-symbols-mode 1)

(add-hook 'after-init-hook (lambda () (load-theme 'modus-operandi-tinted)))
;; modus theme customizations

(defvar my/font-size)

(defun my/customize-mac ()
  (setq my/font-size 120)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(defun my/customize-linux ()
  (setq my/font-size 120))

(if (equal (getenv "HOME") "/Users/icoteril")
    (my/customize-mac)
    (my/customize-linux))

(set-face-attribute 'default nil
		    :family "JetBrains Mono"
		    :height my/font-size)

(set-face-attribute 'variable-pitch nil
		    :family "JetBrains Mono"
		    :height 1.0)

(set-face-attribute 'fixed-pitch nil
		    :family "JetBrains Mono"
		    :height 1.0)

;; Install lambda-themes
(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

;; Install ef-themes
(use-package ef-themes
  :straight (:host nil :repo "https://git.sr.ht/~protesilaos/ef-themes"))

(use-package modus-themes
  :ensure t
  :straight (:host nil :repo "https://git.sr.ht/~protesilaos/modus-themes")
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
      '((fringe unspecified)
	(border-mode-line-active unspecified)
	(border-mode-line-inactive unspecified))))

(use-package all-the-icons
  :ensure t)

;; Modeline

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :ensure t)
(minions-mode)

;; Window/frame setup management

(use-package burly
  :straight (:host github
             :repo "alphapapa/burly.el"))

(global-set-key (kbd "C-v") nil) ; Overrides scroll-up to free prefix
(global-set-key (kbd "C-v b") #'burly-bookmark-windows)
(global-set-key (kbd "C-v B") #'burly-bookmark-frames)
(global-set-key (kbd "C-v v") #'burly-open-bookmark)

;;; Help, Completion

;; marginalia
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode))

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  
;; File management
;; From Emacs From Scratch #10 - Effortless File Management with Dired
;; https://www.youtube.com/watch?v=PMWwM8QJAtU

(use-package dired
  :ensure t
  :straight (:type built-in)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))


(use-package dired-single
  :ensure t
  :after dired)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; dired-open : to map extensions with external programs

;; corfu
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; helpful

(use-package helpful
  :defer t)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

;; consult

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")) ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))


;; orderless

(use-package orderless
  :ensure t
  :custom 
 (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; embark

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package emacs-rotate
  :straight (:type git :host github :repo "daichirata/emacs-rotate"))
  
;;; Core modules

;; TRAMP
(setq tramp-default-method "sshx")
(setq tramp-verbose 10)

;;  tab-bar
(tab-bar-mode t) ; Enable tab-bar

;;; 3rd party Modules

;;; Projectile TODO project discovery, improve search-path load time
(use-package projectile
  :diminish projectile-mode                                                         				
;;  :custom ((projectile-completion-system 'ivy))
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/workspace")
    (setq projectile-project-search-path '("~/workspace"
                                           "~/workspace/cerndb"
                                           "~/workspace/nile"
                                           "~/workspace/kafka"
                                           "~/workspace/monit"	   
                                           "~/workspace/puppet"))))
;; Notes
;  "C-c p b" counsel-projectile-switch-to-buffer
;  "C-c p p" counsel-projectile-switch-project
;  "C-c p s r" counsel-projectile-rg
;  "C-C p c" projectile-compile-project
;  "C-C p K" projectile-package-project

(load "+focus.el")

(load "+term.el")

(load "+git.el")

(load "+pass.el")

(load "+org.el")

(load "+denote.el")

(load "+elfeed.el")

(load "+mail-notmuch.el")

;;(load "+calendar.el")

;;(load "+blog.el")

(load "+irc.el")

(load "+pdf.el")

(load "+programming.el")

;; Keybindings
;(global-set-key (kbd "RET") #'newline)

;; Override forward-char
(global-set-key (kbd "C-f") nil)
(global-set-key (kbd "C-f f") #'consult-find) 
(global-set-key (kbd "C-f r") #'consult-recent-file)
(global-set-key (kbd "C-f P") (lambda () (interactive) (consult-find "~/.emacs.nano"))) 

;; Mail
(global-set-key (kbd "C-x m") #'notmuch-mua-mail)
;; (global-set-key (kbd "C-x m a") #'icot/notmuch-show-process-attachment)
                                                                                   
;; Overrides open-line
(global-set-key (kbd "C-o") nil)
;  "oc" '(counsel-org-capture :which-key "org capture")
(global-set-key (kbd "C-o c") #'org-capture)
(global-set-key (kbd "C-o t") #'org-todo-list)
(global-set-key (kbd "C-o m") #'notmuch-jump-search)

;; Override transpose-char for toggle like settings
(global-set-key (kbd "C-t") nil)

(global-set-key (kbd "C-t l") #'global-display-line-numbers-mode)
(global-set-key (kbd "C-t s") #'eshell)
(global-set-key (kbd "C-t w") #'whitespace-mode)
(global-set-key (kbd "C-t i") #'highlight-indent-guides-mode)
(global-set-key (kbd "C-t z") #'icot/olivetti-mode)
(global-set-key (kbd "C-t t") #'consult-theme)
(global-set-key (kbd "C-t m") #'modus-themes-toggle)
(global-set-key (kbd "C-t e") #'(lambda () (interactive) (eat "/usr/bin/zsh")))
(global-set-key (kbd "C-,") #'multi-vterm-dedicated-toggle)
(global-set-key (kbd "C-t V") #'multi-vterm)

;; C-x t t -> other-tab-prefix. Will execute command in a new tab
(global-set-key (kbd "C-t n") #'tab-bar-new-tab)
(global-set-key (kbd "C-t k") #'tab-bar-close-tab)
(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-next-tab)

(global-set-key (kbd "C-x h") #'windmove-left)
(global-set-key (kbd "C-x j") #'windmove-down)
(global-set-key (kbd "C-x k") #'windmove-up)
(global-set-key (kbd "C-x l") #'windmove-right)

;; "wH" '(evil-window-move-far-left :which-key "move window to the left")
;; "wJ" '(evil-window-move-very-bottom :which-key "move window to bottom")
;; "wK" '(evil-window-move-very-top :which-key "move window to the top")
;; "wL" '(evil-window-move-far-right :which-key "move window to the right")
;; "wn" '(evil-window-new :which-key "new window")

(global-set-key (kbd "C-x |") #'split-window-horizontally)
(global-set-key (kbd "C-x _") #'split-window-vertically)
(global-set-key (kbd "C-x =") #'balance-windows)

(global-set-key (kbd "C-/") #'consult-line)
(global-set-key (kbd "C-\\") #'consult-ripgrep)
(global-set-key (kbd "C-:") #'eval-expression)
(global-set-key (kbd "C-;") #'execute-extended-command)

(global-set-key (kbd "C-x w") #'transpose-frame)

;;;; init.el ends here
(setq gc-cons-threshold (* 2 1000 1000))
;;;; init.el ends here 


