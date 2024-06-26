;; https://systemcrafters.net/emacs-from-scratch/key-bindings-and-evil/

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; set leader key in all states
  (setq evil-default-state 'insert)
  (evil-set-leader nil (kbd "C-SPC"))
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-define-key 'normal 'global (kbd "<leader>;") 'execute-extended-command)
  (evil-define-key 'normal 'global (kbd "<leader>:") 'eval-expression)

  (evil-define-key 'normal 'global (kbd "<leader>hk") 'helpful-key)
  (evil-define-key 'normal 'global (kbd "<leader>hv") 'helpful-variable)
  (evil-define-key 'normal 'global (kbd "<leader>hf") 'helpful-callable)
  
  (evil-define-key 'normal 'global (kbd "<leader>ee") 'eval-last-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>eb") 'eval-buffer)
  
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'windmove-right)
  (evil-define-key 'normal 'global (kbd "<leader>w|") 'split-window-horizontally)
  (evil-define-key 'normal 'global (kbd "<leader>w_") 'split-window-vertically)
  (evil-define-key 'normal 'global (kbd "<leader>w=") 'balance-windows)
  (evil-define-key 'normal 'global (kbd "<leader>w0") 'delete-window)
  
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'ido-switch-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'ido-kill-buffer)
  
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'counsel-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>fr") 'counsel-recentf)
  (evil-define-key 'normal 'global (kbd "<leader>fp") (lambda () (interactive) (counsel-find-file "~/.emacs.d")))

  (evil-define-key 'normal 'global (kbd "<leader>mm") 'notmuch-mua-mail)
  (evil-define-key 'normal 'global (kbd "<leader>mo") 'notmuch)

  ;; toggles
  (evil-define-key 'normal 'global (kbd "<leader>tl") 'global-display-line-numbers-mode)
  (evil-define-key 'normal 'global (kbd "<leader>ts") 'eshell)
  (evil-define-key 'normal 'global (kbd "<leader>tw") 'whitespace-mode)
  ;;(evil-define-key 'normal 'global (kbd "<leader>ti") ')
  (evil-define-key 'normal 'global (kbd "<leader>tz") 'icot/olivetti-mode)
  (evil-define-key 'normal 'global (kbd "<leader>tt") 'consult-theme)
  (evil-define-key 'normal 'global (kbd "<leader>tm") 'modus-themes-toggle)
  ;;(evil-define-key 'normal 'global (kbd "<leader>te") ')
  (evil-define-key 'normal 'global (kbd "<leader>,") 'multi-vterm-dedicated-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>tv") 'multi-vterm)  
  ;; (kbd "c-t i") #'highlight-indent-guides-mode             
  ;; (kbd "c-t e") #'(lambda () (interactive) (eat "/usr/bin/zsh"))
  
  ;; tabs
  (evil-define-key 'normal 'global (kbd "<leader>tn") 'tab-bar-new-tab)
  (evil-define-key 'normal 'global (kbd "<leader>tk") 'tab-bar-close-tab)
  (evil-define-key 'normal 'global (kbd "<leader>th") 'tab-bar-switch-to-prev-tab)
  (evil-define-key 'normal 'global (kbd "<leader>tl") 'tab-bar-switch-to-next-tab)
  
  (evil-define-key 'normal 'global (kbd "<leader>/") #'swiper)
  (evil-define-key 'normal 'global (kbd "<leader>\\") #'counsel-rg)
  (evil-define-key 'normal 'global (kbd "<leader>gg") 'magit-status)
  (evil-mode 1)
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

(add-hook 'notmuch-hello-mode-hook #'turn-off-evil-mode nil)



