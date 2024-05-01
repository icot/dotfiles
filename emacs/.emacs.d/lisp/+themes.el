;; GUI basics

(setq inhibit-startup-screen t)

;; Font size
(defvar my/font-size)

;; https://www.reddit.com/r/emacs/comments/isl1s5/remapping_the_command_key_on_macos_to_ctrl/
;; https://www.reddit.com/r/MacOS/comments/ugelbc/tips_for_macos_modifier_key_remapping_emacs/
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Mac-_002f-GNUstep-Customization.html

(defun my/customize-mac ()
  (setq my/font-size 13)
;	mac-command-modifier 'control
;	ns-alternative-modifier 'meta
;	ns-command-modifier 'control
;	ns-right-alternate-modifier 'meta
;	ns-right-command-modifier 'super)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

(defun my/customize-linux ()
  (setq my/font-size 13)
  ;; Undecorated frames
  (add-to-list 'default-frame-alist '(undecorated . t)))


;; Extra themes


;; Install lambda-themes

(global-hl-line-mode)

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

;; Install ef-themes
(use-package ef-themes
  :straight (:host nil :repo "https://github.com/protesilaos/ef-themes"))

(use-package modus-themes
  :ensure t
  :straight (:host nil :repo "https://github.com/protesilaos/modus-themes")
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
	'((fringe unspecified)
	  (border-mode-line-active bg-mode-line-active)
	  (border-mode-line-inactive bg-mode-line-inactive))))

;;(add-hook 'after-init-hook (lambda () (load-theme 'modus-operandi)))
;; modus theme customizations

;; Doom themes
(use-package doom-themes
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

(use-package all-the-icons
  :ensure t)

;; Modeline customizations

;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :ensure t
  :init
  (minions-mode))


;; Nano testing
(use-package nano
  :straight (:type git :host github :repo "rougier/nano-emacs")
  :config
  (setq nano-font-size 13)
  (setq nano-font-family-monospaced "Jetbrains Mono")
  (require 'nano))

;; This block needs to be after nano loads as it will take over

(if (eq system-type 'darwin) ;; berkeley-unix
    (my/customize-mac)
    (my/customize-linux))

;; Set font to "JetBrains Mono"

(set-face-attribute 'default nil
		    :family "Jetbrains Mono"
		    :height 130)
(set-frame-font (format "Jetbrains Mono %s" my/font-size) nil t)

(menu-bar-mode -1) ; disable menu
(tool-bar-mode -1) ; disable tool-bar
(scroll-bar-mode -1) ; disable scroll-bar
(tab-bar-mode -1)  ; Enable tab-bar

;; (setq tab-bar-separator "|")

(set-fringe-mode 10)
(global-prettify-symbols-mode 1)

(use-package modern-tab-bar
  :straight (:host github :repo "aaronjensen/emacs-modern-tab-bar")
  :init
  (setq tab-bar-show t
        tab-bar-new-button nil
        tab-bar-close-button-show nil)
  (modern-tab-bar-mode))

;; Requires customizization of face (box size to (2.2), doesn't work on dark 

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(modern-tab-bar ((t (:inherit (variable-pitch default) :background "#E0E0E0" :foreground "#000000" :box (:line-width (2 . 2) :style flat-button) :weight light)))))
