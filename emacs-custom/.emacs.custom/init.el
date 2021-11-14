;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ignacio Coterillo
;;
;; Author: Ignacio Coterillo <https://github.com/spike>
;; Maintainer: Ignacio Coterillo <ignacio.coterillo@gmail.com>
;; Created: November 07, 2021
;; Modified: November 07, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/spike/init
;; Package-Requires: ((emacs "24.3"))
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

;;; Basics
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(setq load-prefer-newer t)

;;; Native modes
;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;;; Auto-compile
(setq load-prefer-newer t)
;;(require 'auto-compile)
;;(after! auto-compile
;;  (auto-compile-on-load-mode)
;;  (auto-compile-on-save-mode))


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
;(add-hook 'after-init-hook (lambda () (load-theme 'modus-operandi)))

; requires all-the-icons font
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :init (load-theme 'modus-operandi t))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
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
;; (use-package ligature) ;; FIXME configure from repo

(use-package posframe
  :defer t)

(use-package olivetti
  :defer t)

;;; Completions (Emacs from Scratch #1:https://www.youtube.com/watch?v=74zOY-vgkyw )

;; company

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-h C-t" . counsel-load-theme)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^


;; Improved help  
(use-package helpful)

;;; Tools
(use-package org
  :defer t
  :init
  (setq org-directory "~/NextCloud/myorg/"))

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

(icot/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bb" '(counsel-ibuffer :which-key "counsel-ibuffer")
  "e" '(:ignore t :which-key "eval")
  "eb" '(eval-buffer :which-key "eval buffer")
  "el" '(eval-last-sexp :which-key "eval last sexp")
  "f" '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "recent-files")
  "fP" '((lambda () ((interactive)
		     (counsel-find-file nil "~/.emacs.custom"))) :which-key "Config folder") ;; FIXME
  "g" '(:ignore t :which-key "git")
  "gg" '(magit-status :which-key "magit-status")
  "h" '(:ignore t :which-key "help")
  "hk" '(helpful-key :which-key "help key")
  "hf" '(helpful-function :which-key "help function")
  "hv" '(helpful-variable :which-key "help variable")
  "p" '(:ignore t :which-key "projectile")
  "pp" '(projectile-switch-project :which-key "projectile-switch-project")
  "t" '(:ignore t :which-key "toggles")
  "tp" '(ivy-pass :which-key "pass")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "tz" '(olivetti-mode :which-key "olivetti mode")
  "tZ" '(olivetti-mode :which-key "olivetti full-screen") ;; TODO
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
  ";" '(counsel-M-x :which-key "counsel-M-x")
  "/" '(counsel-rg :which-key "counsel-rg") ;; FIXME: effective directory
  ":" '(eval-expression :which-key "eval-expresion"))
 
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
;;  (setq projectile-switch-project-action '#'counsel-find-file)) ;; FIXME

;;; Languages
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :defer t)

;; FIXME init/config
(use-package drag-stuff
  :diminish t
  :init
  (setq drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

;;; Terms
;; TODO vterm
;; TODO eshell 

;;; TOOLS 

(add-to-list 'load-path "~/.emacs.custom/lisp")

;;; Auth/pass
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
;; (load "+irc.el") ;; FIXME: doom macros (use-package!)

;; org-blog
; (load "+blog") ;; FIXME: package installation (manual?)

;; pdf-tools
(use-package pdf-tools) 

;; TRAMP
(setq tramp-default-method "sshx")
(setq tramp-verbose 10)

;;; Programming language and tools
(load "+programming.el")

;;; init.el ends here
