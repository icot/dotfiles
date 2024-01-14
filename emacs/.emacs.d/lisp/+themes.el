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
  :config
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-common-palette-overrides
	'((fringe unspecified
		  (border-mode-line-active unspecified)
		  (border-mode-line-inactive unspecified)))))

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



