;; Misc configuration


(setq mail-user-agent 'mu4e-user-agent
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-envelope-from 'header
      sendmail-program "/usr/bin/msmtp")

;;; Set up some common mu4e variables
(setq mu4e-maildir "~/mail"
      mu4e-debug t
      mu4e-update-interval 180
      mu4e-headers-auto-update t
      mu4e-headers-skip-duplicates nil
      mu4e-compose-signature-auto-include nil
      mu4e-compose-in-new-frame nil
      mu4e-compose-dont-reply-to-self t
      mu4e-compose-context-policy nil
      mu4e-use-fancy-chars t
      mu4e-view-show-images t
      mu4e-view-show-addresses t
      mu4e-hide-index-messages t
      message-kill-buffer-on-exit t
      )

(setq mu4e-account-alist
      '(
        ("CERN"
         (mu4e-sent-messages-behavior sent)
         (mu4e-sent-folder "/CERN/SentItems")
         (mu4e-drafts-folder "/CERN/Drafts")
         (mu4e-trash-folder "/CERN/Trash")
         (mu4e-refile-folder "/CERN/Archives/2020")
         (user-mail-address "ignacio.coterillo.coz@cern.ch")
         (user-full-name "Ignacio Coterillo Coz"))
        ("gmail"
         ;; Under each account, set the account-specific variables you want.
         (mu4e-sent-messages-behavior delete)
         (mu4e-sent-folder "/gmail/[Gmail]/Sent Mail")
         (mu4e-trash-folder "/gmail/[Gmail]/Trash")
         (mu4e-drafts-folder "/gmail/[Gmail]/Drafts")
         (mu4e-refile-folder "/gmail/[Gmail]/All Mail")
         (user-mail-address "ignacio.coterillo@gmail.com")
         (user-full-name "Ignacio Coterillo Coz"))
       ))

;; https://www.djcbsoftware.nl/code/mu/mu4e/Contexts-example.html
(setq mu4e-contexts
   `(,(make-mu4e-context
        :name "CERN"
        :enter-func (lambda () (mu4e-message "Switch to the Work context"))
        :leave-func (lambda () (mu4e-message "Leaving Work context"))
        ;; no leave-func
        ;; we match based on the maildir of the message
        ;; this matches maildir /Arkham and its sub-directories
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/CERN" (mu4e-message-field msg :maildir))))
        :vars '(
                ( user-mail-address . "ignacio.coterillo.coz@cern.ch" )
                ( user-full-name . "Ignacio Coterillo Coz" )
                ( mu4e-sent-messages-behavior . sent)
                ( mu4e-sent-folder . "/CERN/SentItems")
                ( mu4e-drafts-folder . "/CERN/Drafts")
                ( mu4e-trash-folder . "/CERN/Trash")
                ( mu4e-refile-folder . "/CERN/Archives/2020")
                ( mu4e-compose-signature  .
                                          (concat
                                           "Ignacio Coterillo\n"
                                           "IT-DB-IA\n"))))
     ,(make-mu4e-context
        :name "gmail"
        :enter-func (lambda () (mu4e-message "Entering Private context"))
        :leave-func (lambda () (mu4e-message "Leaving Private context"))
        ;; we match based on the contact-fields of the message
        :match-func (lambda (msg)
                      (when msg
                        (string-match-p "^/gmail" (mu4e-message-field msg :maildir))))
        :vars '(
                ( user-mail-address	. "ignacio.coterillo@gmail.com"  )
                ( user-full-name . "Ignacio  Coterillo" )
                ( mu4e-sent-messages-behavior . delete)
                ( mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                ( mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                ( mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                ( mu4e-refile-folder . "/gmail/[Gmail]/All Mail")
                ( mu4e-compose-signature . nil )))
      ))

;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.
;; start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

;; compose with the current context if no context matches;
;; default is to ask
;; (setq mu4e-compose-context-policy nil)

;; https://www.djcbsoftware.nl/code/mu/mu4e/Compose-hooks.html
(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (when msg
	(setq user-mail-address
	  (cond
	    ((mu4e-message-contact-field-matches msg :to "ignacio.coterillo@gmail.com")
	      "ignacio.coterillo@gmail.com")
	    (t "ignacio.coterillo.coz@cern.ch")))))))


(setq mu4e-change-filenames-when-moving t)


(add-to-list 'mu4e-header-info-custom
 '(:full-mailing-list .
     ( :name "Mailing-list"               ;; long name, as seen in the message-view
       :shortname "ML"                    ;; short name, as seen in the headers view
       :help "Full name for mailing list" ;; tooltip
       :function (lambda (msg)
           (or (mu4e-message-field msg :mailing-list) "")))))

;; (map! :leader :desc "Dired fuzzy search" :nv "d" #'dired)

;; Thread folding
;; https://github.com/rougier/mu4e-thread-folding

(load! "mail/mu4e-thread-folding")

(add-to-list 'mu4e-header-info-custom
             '(:empty . (:name "Empty"
                         :shortname ""
                         :function (lambda (msg) "  "))))

(setq mu4e-headers-fields '((:empty         .    2)
                            (:human-date    .   12)
                            (:flags         .    6)
                            (:mailing-list  .   20)
;                            (:decryption  .   20)
;                            (:signature  .   20)
                            (:from          .   28)
                            (:subject       .   nil)))

(define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
(define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
(define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)

;; Signature verification on S/MIME emails can freeze due
;; to CRL verification: See https://github.com/djcb/mu/issues/1501

;;  Fix: disable-crl-checks on ./gnupg/gpgsm.conf

