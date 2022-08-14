;;; pass/Auth/pass

(use-package pass
  :defer t)
(use-package password-store
  :defer t)

;; Set Auth Source to use pass
;;   https://www.gnu.org/software/emacs/manual/html_mono/auth.html#Top
(setq auth-sources '(password-store))
