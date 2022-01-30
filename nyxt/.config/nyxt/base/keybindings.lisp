(in-package #:nyxt-user)

;;keybinding overrides
(define-configuration buffer
  ((override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                     "o" 'set-url-new-buffer
                     "d" 'delete-buffer
                     "j" 'nyxt/web-mode:scroll-down
                     "k" 'nyxt/web-mode:scroll-up
                     "M-h" 'nyxt/web-mode:scroll-left
                     "M-l" 'nyxt/web-mode:scroll-right
                     "f" 'nyxt/web-mode:follow-hint
                     "F" 'nyxt/web-mode:follow-hint-new-buffer
                     "h f" 'describe-function
                     "h v" 'describe-variable
                     "h b" 'describe-bindings
                     "h c" 'describe-command
                     "M-x" 'execute-command
                     "C-s" 'query-selection-in-search-engine
                     "C-c" 'nyxt/web-mode::copy ;;currently an override because C-c not working without it
                     "C-q" 'quit-history)
                   map))))
