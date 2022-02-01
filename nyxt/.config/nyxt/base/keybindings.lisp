(in-package #:nyxt-user)

;;keybinding overrides
(define-configuration buffer
  ((override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                     "C-d" 'delete-buffer
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
