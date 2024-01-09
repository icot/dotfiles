;; https://systemcrafters.net/emacs-from-scratch/key-bindings-and-evil/

(use-package evil
  :ensure t
  :config
  ;; set leader key in all states
  (evil-set-leader nil (kbd "C-SPC"))
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC")))
  
(evil-define-key 'normal 'global (kbd "<leader>;") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "<leader>:") 'eval-expression)
(evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
(evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
(evil-define-key 'normal 'global (kbd "<leader>w|") 'split-window-horizontally)
(evil-define-key 'normal 'global (kbd "<leader>w_") 'split-window-vertically)
(evil-define-key 'normal 'global (kbd "<leader>w=") 'balance-windows)

;; "wH" '(evil-window-move-far-left :which-key "move window to the left")
;; "wJ" '(evil-window-move-very-bottom :which-key "move window to bottom")
;; "wK" '(evil-window-move-very-top :which-key "move window to the top")
;; "wL" '(evil-window-move-far-right :which-key "move window to the right")
;; "wn" '(evil-window-new :which-key "new window")

  
