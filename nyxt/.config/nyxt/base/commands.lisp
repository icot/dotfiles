(in-package #:nyxt-user)

;;shows current time
(define-command-global current-time ()
  "Show the current time."
  (echo "~a" (local-time:now)))

;;opens current url in different browser replace firefox with your browser/path
(define-command-global open-external-browser ()
  "Open the current url in external browser"
  (uiop:run-program (list "firefox" (render-url (url (current-buffer))))))
