;;; init.el --- Description -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2021 Ignacio Coterillo
;;
;; Author: Ignacio Coterillo <https://github.com/icot>
;; Maintainer: Ignacio Coterillo <ignacio.coterillo@gmail.com>
;; Created: November 07, 2021
;; Modified: November 18, 2021
;; Version: 0.1
;; Homepage: https://github.com/icot/dotfiles
;; Package-Requires: ((emacs "28.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;; https://github.com/anderspollack/emacs-straight/blob/main/init.el
;; https://systemcrafters.cc/advanced-package-management/using-straight-el/

(provide 'init)

(add-to-list 'load-path "~/.emacs.custom/lisp")

;; Fromh ttps://github.com/daviwil/emacs-from-scratch/blob/master/init.el 
(defun icot/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'icot/display-startup-time)


;; https://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs
(defun icot/package-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))

;;; Basics
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(setq load-prefer-newer t)

;;; Native modes
;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)


;; Store customizations in separate file
(setq custom-file "~/.emacs.custom/custom.el")
(load custom-file)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ignacio Coterillo"
      user-mail-address "ignacio.coterillo@gmail.com")

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

;;; Init benchmarking
;; https://github.com/dholm/benchmark-init-el

;; Error (use-package): benchmark-init/:catch: Wrong number of arguments: (3 . 4), 2 Disable showing Disable logging
;; https://github.com/hlissner/doom-emacs/issues/4534
;; https://gist.github.com/hlissner/175c21000114d1dba7a47678191a888a
(load "wrong-args-fix.el")

(use-package benchmark-init
 :ensure f
  :config
;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;;; Theme, Fonts, UI

;; Disable menu bar and scroll bar
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(set-fringe-mode 10)

;; visible bell
(setq visible-bell t)
(set-face-attribute 'default nil :height 120)
(global-hl-line-mode)
(add-hook 'after-init-hook (lambda () (load-theme 'modus-vivendi)))

; requires all-the-icons font
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Line numbers
(column-number-mode)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		help-mode-hook
		magit-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))) 

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

;; TODO ligatures
;; https://www.masteringemacs.org/article/unicode-ligatures-color-emoji
(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package posframe
  :defer t)

;; FIXME font-size increase/decrease
(use-package writeroom-mode
  :defer t
  :init
  (setq writeroom-width 120))
;;  :config
;;  (add-hook 'writeroom-mode-hook #'text-scale-increase)
;;  (add-hook 'writeroom-mode-hook #'global-display-line-numbers-mode)
;;  (add-hook 'change-major-mode-hook #'text-scale-decrease)
;;  (add-hook 'change-major-mode-hook #'global-display-line-numbers-mode))

;;(use-package rg
;;  :defer t)

;;; Completions (Emacs from Scratch #1:https://www.youtube.com/watch?v=74zOY-vgkyw )
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-posframe
  :init
  (ivy-posframe-mode 1))
;  :config
;  (setq ivy-posframe-parameters
;	'((left-fringe . 8)
;	  (right-fring . 8))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-h C-t" . counsel-load-theme)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy-rich
  :after counsel
  :init
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; Improved help  
(use-package helpful)

;;; Tools
(use-package org
  :defer t
  :init
  (setq org-directory "~/Nextcloud/myorg/")
  (setq org-agenda-files `(,org-directory)))
			  

(use-package magit
  :defer t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; Keybindings
;; Hydra -> Transient keybindings
(use-package general)
(general-create-definer icot/leader-keys
			:keymaps '(normal insert visual emacs)
			:prefix "SPC"
			:global-prefix "C-SPC")

(defun icot/open-config-folder ()
  (interactive)
  (counsel-find-file nil "~/.emacs.custom"))
  
(icot/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bb" '(ivy-switch-buffer :which-key "ivy-switch-buffer")
  "e" '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval buffer")
  "el" '(eval-last-sexp :which-key "eval last sexp")
  "f" '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "recent-files")
  "fP" '(icot/open-config-folder :which-key "Open config folder")
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "magit-status")
  "h" '(:ignore t :which-key "help")
  "hk" '(helpful-key :which-key "help key")
  "hf" '(helpful-function :which-key "help function")
  "hv" '(helpful-variable :which-key "help variable")
  "o" '(:ignore t :which-key "open")
  "ot" '(org-todo-list :which-key "org TODO list")
  "om" '(notmuch-jump-search :which-key "notmuch") ;; Requires load binding to this method
  "p" '(:ignore t :which-key "projectile")
  "pp" '(projectile-switch-project :which-key "projectile-switch-project")
  "t" '(:ignore t :which-key "toggles")
  "tl" '(global-display-line-numbers-mode :which-key "line numbers")
  "tp" '(ivy-pass :which-key "pass")
  "tc" '(counsel-load-theme :which-key "choose color theme")
  "ts" '(eshell :which-key "toggle terminal (eshell)")
  "tz" '(writeroom-mode :which-key "writeroom mode")
  "w" '(:ignore t :which-key "window")
  "wh" '(evil-window-left :which-key "switch to left window")
  "wj" '(evil-window-down :which-key "switch to bottom window")
  "wk" '(evil-window-up :which-key "switch to top window")
  "wl" '(evil-window-right :which-key "switch to right window")
  "wH" '(evil-window-move-far-left :which-key "move window to the left")
  "wJ" '(evil-window-move-very-bottom :which-key "move window to bottom")
  "wK" '(evil-window-move-very-top :which-key "move window to the top")
  "wL" '(evil-window-move-far-right :which-key "move window to the right")
  "w_" '(evil-window-split :which-key "window split horizontal")
  "w|" '(evil-window-vsplit :which-key "window split vertical")
  "wn" '(evil-window-new :which-key "new window")
  ":" '(counsel-M-x :which-key "counsel-M-x")
;;  "/" '(projectile-ripgrep :which-key "counsel-rg") ;; TODO copy doom behaviour
  ";" '(eval-expression :which-key "eval-expresion"))
 
; (define-key keymap key def)
(defun icot/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; necessary for evil-collection
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  ;; (setq evil-want-C-u-scroll t)
  ;; :hook (evil-mode . icot/evil-hook)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; Visually show areas of operation
(use-package evil-goggles)

;; Text alignment with gl and gL
(use-package evil-lion
  :diminish t
  :ensure t
  :config
  (evil-lion-mode))

;; evil-snipe: TODO check evil f/F/t/T

;;; Projectile
(use-package projectile
  :diminish projectile-mode
  :custom ((projectile-completion-system 'ivy))
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/workspace")
    (setq projectile-project-search-path '("~/workspace"
					   "~/workspace/cerndb"
					   "~/workspace/puppet"))))

;;; Languages
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :defer t)

;; drag-stuff
(use-package drag-stuff
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

;;; Terms
(use-package vterm
  :ensure t
  :defer t
  :config
  (setq display-buffer-alist '(("\\`\\*vterm" display-buffer-pop-up-window))))

;; For new frame: display-buffer-pop-up-frame
(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

;;; TOOLS 

;; Auth/pass
(use-package pass
  :defer t)
(use-package password-store
  :defer t)
(use-package ivy-pass
  :defer t)

;; Set Auth Source to use pass
;;   https://www.gnu.org/software/emacs/manual/html_mono/auth.html#Top
(setq auth-sources '(password-store))

;; RSS
(load "+elfeed.el")

;; Mail
(load "+mail-notmuch.el")

;; IRC
(load "+irc.el")

;; org-blog
(load "+blog.el")

;; pdf-tools
(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install))

;; TRAMP
(setq tramp-default-method "sshx")
(setq tramp-verbose 10)

;;; Programming language and tools
(load "+programming.el")

;;; Reset gc-cons-threshold
(setq gc-cons-threshold (* 2 1000 1000))
;;; init.el ends here
