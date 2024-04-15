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

(add-hook 'after-init-hook (lambda () (load-theme 'modus-operandi)))
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
(straight-use-package
 '(nano :type git :host github :repo "rougier/nano-emacs"))

;; (require 'nano)

(menu-bar-mode -1) ; disable menu
(tool-bar-mode -1) ; disable tool-bar
(scroll-bar-mode -1) ; disable scroll-bar
(tab-bar-mode t) ; Enable tab-bar

(setq tab-bar-separator "|")


(set-fringe-mode 10)
(global-prettify-symbols-mode 1)
