;; eshell/term/vterm/eat configuration

;; Helper functions

(defun icot/clean-window ()
  (interactive)
  (message "Cleaning window")
  (if (one-window-p t t)
      (delete-frame)
      (delete-window)))

(defun icot/vterm-handle-exit (&optional process-name msg)
  (message "icot/vterm-handle-exit %s | %s" process-name msg)
  (kill-buffer (current-buffer))
  (delete-frame))

;;; EAT https://codeberg.org/akib/emacs-eat
(straight-use-package
 '(eat :type git
       :host nil
       :repo "https://codeberg.org/akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(defvar eat-session-id 0)

(defun icot/multi-eat ()
  (interactive)
  (setq eat-session-id (+ eat-session-id 1))
  (eat "/usr/bin/zsh" eat-session-id))

;;; vterm

(setq eat-kill-buffer-on-exit t)
(add-hook 'eat-exit-hook #'icot/vterm-handle-exit)

;; vterm
(use-package vterm
  :ensure t
  :config
  (add-to-list 'vterm-keymap-exceptions "C-t")
  (add-to-list 'vterm-keymap-exceptions "C-;")
  (add-to-list 'vterm-exit-functions 'icot/vterm-handle-exit))

(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  (setq multi-vterm-program "/usr/bin/zsh"))

;; Misc

(defun icot/shell-mode-hook ()
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

;;  Call cleanup helpers after exit
(advice-add 'term-handle-exit :after 'icot/vterm-handle-exit)
(advice-add 'eshell-life-is-too-much :after 'icot/clean-window)


;;; Hooks

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)
;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(add-hook 'shell-mode-hook 'icot/shell-mode-hook)

;; Hide mode line
(use-package hide-mode-line
  :ensure t)

(add-hook 'vterm-mode-hook #'hide-mode-line-mode)
(add-hook 'eat-mode-hook #'hide-mode-line-mode)
(add-hook 'eshell-mode-hook #'hide-mode-line-mode)

;;;  Display in new split buffer/frames
(setq display-buffer-alist
      '(("\\`\\*e?shell" display-buffer-pop-up-window)
        ("\\`\\*eat*" display-buffer-pop-up-frame)
        ("\\`\\*term*" display-buffer-pop-up-frame)
        ("\\`\\*vterminal.*$" display-buffer-pop-up-frame) ;; https://github.com/suonlight/multi-vterm/issues/23
	("\\`\\*vterm" display-buffer-pop-up-frame)))
