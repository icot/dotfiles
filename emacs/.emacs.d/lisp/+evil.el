
(use-package evil
  :ensure t
  :config
  ;; set leader key in all states
  (evil-set-leader nil (kbd "C-SPC"))
  ;; set leader key in normal state
  (evil-set-leader 'normal (kbd "SPC")))
  
(evil-define-key 'normal 'global (kbd "<leader>;") 'execute-extended-command)
(evil-define-key 'normal 'global (kbd "<leader>:") 'eval-expression)



  
