(in-package #:nyxt-user)

;;loading of config files
;;base
(nyxt::load-lisp "~/.config/nyxt/base/keybindings.lisp")
(nyxt::load-lisp "~/.config/nyxt/base/search.lisp")
(nyxt::load-lisp "~/.config/nyxt/base/commands.lisp")
;;extending
;(nyxt::load-lisp "~/.config/nyxt/ex/specificurl.lisp")

;;configuration for browser
(define-configuration browser
  ((session-restore-prompt :never-restore)))

;; Configuring Default modes
(define-configuration buffer
  ((default-modes (append '(reduce-tracking-mode ;; Tracking
                            blocker-mode ;; Ad blocking
                                        ;    nyxt::vi-normal-mode) ;; VI Mode
                            )
                          %slot-default%))))

;;setting new buffer url and having nyxt start full screen
;(defmethod nyxt::startup ((browser browser) urls)
;  "Home"
;  (window-make browser)
;  (let ((window (current-window)))
;    (window-set-buffer window (make-buffer :url (quri:uri "https://nyxt.atlas.engineer/")))
;    (toggle-fullscreen window)))


(echo "Start swank server")
(start-swank)

;;when reloading init.lisp file shows in message bar once finished
(echo "Loaded config.")
