;; eshell/term/vterm/eat configuration

;; Helper functions

(defun my/clean-window ()
  (interactive)
  (if (one-window-p t t)
      (delete-frame)
      (delete-window)))

(defun my/term-handle-exit (&optional process-name msg)
  (message "%s | %s" process-name msg)
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

(defun my/multi-eat ()
  (interactive)
  (setq eat-session-id (+ eat-session-id 1))
  (eat "/usr/bin/zsh" eat-session-id))

;;; vterm

;; TODO Function to close tab if vterm buffer is the only buffer
;; Add to vterm-exit-functions

(use-package vterm
  :ensure t
  :config
  (add-to-list 'vterm-keymap-exceptions "C-t")
  (add-to-list 'vterm-keymap-exceptions "C-;")
  (add-to-list 'vterm-exit-functions 'my/term-handle-exit))

(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  (setq multi-vterm-program "/usr/bin/zsh"))

(add-to-list 'display-buffer-alist
	     '(("\\`\\*vterm" display-buffer-pop-up-window))
	     t)

;; Misc

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

;;  Call cleanup helpers after exit
;(add-to-list 'vterm-exit-functions 'my/term-handle-exit) ;; Done in use-package
(advice-add 'term-handle-exit :after 'my/term-handle-exit)
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
;;(add-hook 'eat-mode-hook 'my/shell-mode-hook) ;; Doesn't work

;;;  Display in new split buffer
(setq display-buffer-alist
      '(("\\`\\*e?shell" display-buffer-pop-up-window)
        ("\\`\\*eat*" display-buffer-pop-up-frame)
        ("\\`\\*term*" display-buffer-pop-up-frame)
        ("\\`\\*vterminal.*$" display-buffer-pop-up-frame) ;; https://github.com/suonlight/multi-vterm/issues/23
	("\\`\\*vterm" display-buffer-pop-up-frame)))

;;; Misc

;; term - $HOME/apps ls -lR: 11.238s
;; vterm - $HOME/apps ls -lR: 0.473s
;; eshell - $HOME/apps ls -lR: 5.644 (output in block after completion, doesn't scroll)
;; eat - $HOME/apps ls -lR: 5.628s

;; wezterm - 0.153s
;; alacritty - 0.149s
;; kitty - 0.141s
