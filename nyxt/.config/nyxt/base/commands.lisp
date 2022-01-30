(in-package #:nyxt-user)

;;shows current time
(define-command-global current-time ()
  "Show the current time."
  (echo "~a" (local-time:now)))

;;command to close nyxt and delete history file | investigating below issues
;;currently cannot be last command in this list and won't show in commands list so setting a keybinding is necessary
(define-command-global quit-history ()
  (uiop:delete-file-if-exists (expand-data-path (data-profile (current-buffer)) ;;quit and clear history
                                          (history-path (current-buffer))))
  (quit))

;;opens current url in different browser replace firefox with your browser/path
(define-command-global open-external-browser ()
  "Open the current url in external browser"
  (uiop:run-program (list "firefox" (render-url (url (current-buffer))))))
