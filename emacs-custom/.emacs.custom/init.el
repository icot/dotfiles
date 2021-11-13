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
;;; Code:

;; https://github.com/anderspollack/emacs-straight/blob/main/init.el
;; https://systemcrafters.cc/advanced-package-management/using-straight-el/

(provide 'init)

;;; Basics
(setq package-native-compile t)
(setq comp-deferred-compilation t)
(setq load-prefer-newer t)

;;;; Auto-compile
;;(setq load-prefer-newer t)
;;(require 'auto-compile)
;;(after! auto-compile
;;  (auto-compile-on-load-mode)
;;  (auto-compile-on-save-mode))


;; Store customizations in separate file
(setq custom-file "~/.emacs.custom/custom.el")
(load custom-file)

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

;; hl-todo
(use-package hl-todo
  :config
  (setq global-hl-todo-mode 1))

;; TODO ligatures 
;; (use-package ligature) ;; FIXME configure from repo

; TODO posframe

(use-package olivetti)

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
  :init
  (ivy-rich-mode 1))

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
  "tt" '(counsel-load-theme :which-key "choose theme")
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

;; Hydra -> Transient keybindings

;; From doom evil config
(use-package evil-goggles)
;; evil-nerd-commenter
;; evil-easymotion
;; evil-lion
;; evil-snipe
;; evil-embrace, evil-surround

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

;; smartparents/peredit?

;;; Terms
;; TODO vterm
;; TODO eshell 

;; TRAMP
(setq tramp-default-method "sshx")
(setq tramp-verbose 10)

;;; init.el ends here
