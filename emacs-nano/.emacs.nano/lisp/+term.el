;; eshell/term/vterm/eat configuration

(defun my/clean-window ()
  (when (not (one-window-p))
    (delete-window)))

;; EAT https://codeberg.org/akib/emacs-eat
(straight-use-package
 '(eat :type git
       :host nil
       :repo "https://codeberg.org/akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; vterm
(use-package vterm
  :ensure t
  :config
  (add-to-list 'vterm-keymap-exceptions "C-t")
  (add-to-list 'vterm-keymap-exceptions "C-;")
  (add-to-list 'vterm-exit-functions 'my/clean-window))

(use-package multi-vterm
  :ensure t
  :after vterm)

;; EAT

(defvar eat-session-id 0)

(defun my/multi-eat ()
  (interactive)
  (setq eat-session-id (+ eat-session-id 1))
  (eat "/usr/bin/zsh" eat-session-id))

(defun my/shell-mode-hook ()
  "Custom `shell-mode' behaviours."
  ;; Kill the buffer when the shell process exits.
  (let* ((proc (get-buffer-process (current-buffer)))
         (sentinel (process-sentinel proc)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        ;; Call the original process sentinel first.
        (funcall #',sentinel process signal)
        ;; Kill the buffer on an exit signal.
        (progn
          (message ">> Executing shell-mode-hook")
          (and (memq (process-status process) '(exit signal))
           (buffer-live-p (process-buffer process))
           (kill-buffer (process-buffer process))))))))

(advice-add 'eshell-life-is-too-much :after 'my/clean-window)

;;; Hooks

;; TODO Disable mode-line in vterm/eat/term/eshell windows
;; (add-hook 'term-mode-hook (lambda () (interactive) (icot/toggle-hide-mode-line)))

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

;; WIP
(add-hook 'shell-mode-hook 'my/shell-mode-hook) ;; Works
(add-hook 'eat-mode-hook 'my/shell-mode-hook) ;; Doesn't work

;;;  Display in new split buffer
(setq display-buffer-alist
      '(("\\`\\*e?shell" display-buffer-pop-up-window)
        ("\\`\\*eat*" display-buffer-pop-up-frame)
	("\\`\\*vterm" display-buffer-pop-up-window)))

