;; Focus mode and related settings

;; https://protesilaos.com/codelog/2020-07-18-emacs-concept-org-tweaked-focus/

;; https://gist.github.com/rnkn/a522429ed7e784ae091b8760f416ecf8


;; Highlight TODO related keywords
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(defun icot/toggle-hide-mode-line ()
  "Toggle mode-line visibility in current buffer."
  (interactive)
  (if mode-line-format
      (setq-local mode-line-format nil)
    (kill-local-variable 'mode-line-format)))

(use-package olivetti
  :diminish
  :config
  (setq olivetti-body-width 0.65
        olivetti-minimum-body-width 80
        olivetti-recall-visual-line-mode-entry-state t)
  (define-minor-mode icot/olivetti-mode
    "additional olivetti mode parameters"
    :init-value nil
    :global nil
    (if (bound-and-true-p icot/olivetti-mode)
        (progn
         (olivetti-mode 1)
         (icot/toggle-hide-mode-line)
         (text-scale-increase 2))
      (progn
        (text-scale-decrease 2)
        (icot/toggle-hide-mode-line)
        (olivetti-mode -1)))))


