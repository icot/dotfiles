(in-package #:nyxt-user)

;;keybinding overrides 
(define-configuration buffer
  ((override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                     "C-d" 'delete-buffer
                     "C-b" 'switch-buffer
                     "C-B" 'list-bookmarks
                     "C-H" 'nyxt/web-mode:list-history
                     "C-j" 'nyxt/web-mode:scroll-down
                     "C-k" 'nyxt/web-mode:scroll-up
                     "M-h" 'nyxt/web-mode:scroll-left
                     "M-l" 'nyxt/web-mode:scroll-right
                     "C-f" 'nyxt/web-mode:follow-hint
                     "C-F" 'nyxt/web-mode:follow-hint-new-buffer
                     "C-h f" 'describe-function
                     "C-h v" 'describe-variable
                     "C-h b" 'describe-bindings
                     "C-h c" 'describe-command
                     "M-x" 'execute-command
                     "C-s" 'query-selection-in-search-engine
                     "C-c" 'nyxt/web-mode::copy) ;;currently an override because C-c not working without it
                   map))))

;; ;; https://discourse.atlas.engineer/t/right-way-to-modify-keybindings-in-visual-mode/228
;; ;; visual-mode is configurable, but you need to use proper package prefix for that.
;; (define-configuration nyxt/visual-mode:visual-mode
;;   ((keymap-scheme (let ((scheme %slot-default%))
;;                     ;; Use scheme:cua or scheme:vi-normal, if that strikes your fancy.
;;                     (keymap:define-key (gethash scheme:emacs scheme)
;;                       ;; Put as much keybindings as you want here.
;;                       "M-C-s-z" 'reload-current-buffer)
;;                     scheme))))

;; (define-configuration nyxt/vi-mode:vi-normal-mode
;;   ((keymap-scheme (let ((scheme %slot-default%))
;;                     ;; Use scheme:cua or scheme:vi-normal, if that strikes your fancy.
;;                     (keymap:define-key (gethash scheme:vi-normal scheme)
;;                       ;; Put as much keybindings as you want here.
;;                       "d" 'delete-buffer
;;                       "b b" 'switch-buffer
;;                       "B" 'list-bookmarks
;;                       "H" 'nyxt/web-mode:list-history
;;                       "M-h" 'nyxt/web-mode:scroll-left
;;                       "M-l" 'nyxt/web-mode:scroll-right
;;                       "h f" 'describe-function
;;                       "h v" 'describe-variable
;;                       "h b" 'describe-bindings
;;                       "h c" 'describe-command
;;                       "h a" 'describe-any
;;                       "h m" 'describe-macro
;;                       "M-x" 'execute-command
;;                       "C-s" 'query-selection-in-search-engine)
;;                     scheme))))
