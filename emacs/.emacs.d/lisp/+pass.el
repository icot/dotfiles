;;; pass/Auth/pass

(use-package pass
  :defer t)
(use-package password-store
  :defer t)

;; Set Auth Source to use pass
;;   https://www.gnu.org/software/emacs/manual/html_mono/auth.html#Top
(setq auth-sources '(password-store))

;; Copy/Paste in Wayland
(setq wl-copy-process nil)

(defun wl-copy (text)
  (setq wl-copy-process (make-process :name "wl-copy"
                                      :buffer nil
                                      :command '("wl-copy" "-f" "-n")
                                      :connection-type 'pipe
                                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil) ; should return nil if we're the current paste owner
  (shell-command-to-string "wl-paste -n | tr -d \r"))


(cond ((eq system-type 'linux) (setq interprogram-cut-function 'wl-copy
				     interprogram-paste-function 'wl-paste))
      ((eq system-type 'berkeley-unix) nil))

;; External callee 
(defun icot/passmenu ()
  (let ((frame (selected-frame)))
   (message "frame: %s" frame)
   (call-interactively 'password-store-copy)
   (wl-copy (car kill-ring))
   (delete-frame frame)
   0))
   


