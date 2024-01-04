;;; pass/Auth/pass

(use-package pass
  :defer t)
(use-package password-store
  :defer t)

;; Set Auth Source to use pass
;;   https://www.gnu.org/software/emacs/manual/html_mono/auth.html#Top
(setq auth-sources '(password-store))


(defun icot/passmenu ()
  (let ((frame (make-frame '((minibuffer . only)))))
    (message "frame: %s" frame)
    (call-interactively 'password-store-copy)
    (delete-frame frame 't)))

;; (let ((frame (make-frame '((minibuffer . only)))))
;;   (message "frame: %s" frame)
;;   (call-interactively 'password-store-copy)
;;   (delete-frame frame 't))

;; (defun switch-to-minibuffer ()
;;   "Switch to minibuffer window."
;;   (interactive)
;;   (if (active-minibuffer-window)
;;       (select-window (active-minibuffer-window))
;;     (error "Minibuffer is not active")))

;; (global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

;; (frame-list)
;; (frame-focus)


