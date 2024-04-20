(use-package gnus-alias
  :defer t
  :config
  (setq gnus-alias-identity-alist
	'(("gmail"
	     nil ;; Does not refer to any other identity
             "Ignacio Coterillo <ignacio.coterillo@gmail.com>" ;; Sender address
             nil ;; No organization header
             nil ;; No extra headers
             nil ;; No extra body text
             nil );"~/.signature")
	  ("cern"
	    nil
	    "Ignacio Coterillo Coz <ignacio.coterillo.coz@cern.ch>"
	    nil
	    nil ;; No extra headers
	    nil ;; No extra body text
	    nil)); "~/.signature.work"))
        ;; Use "cern" identity by default
        gnus-alias-default-identity "gmail"
    ;; Define rules to match work identity
        gnus-alias-identity-rules
        '(("cern-rule"
	   ("any" "\\(.+\\)@cern.ch" both) "cern")))
  ;; Determine identity when message-mode loads
  :hook (message-setup . gnus-alias-determine-identity))

(defun icot/my-set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "gmail.com" from) "gmail")
               ((string-match "cern.ch" from) "cern"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'icot/my-set-msmtp-account)

(defvar my/smtp (if (eq system-type 'berkeley-unix)
                    "/usr/local/bin/msmtp"
                    "/usb/bin/msmtp"))
                  
;; https://notmuchmail.org/emacstips/#index11h2
(setq message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      message-kill-buffer-on-exit t
      mail-user-agent 'gnus-user-agent
      sendmail-program my/smtp)

;; LDAP address book
(with-eval-after-load "message"
  (define-key message-mode-map (kbd "TAB") 'eudc-expand-try-all))
  
(setopt eudc-server-hotlist
        '(("ldaps://ldap.cern.ch" . ldap)))

(setopt ldap-host-parameters-alist
        `(("ldaps://ldap.cern.ch:636"
                  base "o=cern,c=ch"
                  binddn "cn=icoteril,ou=users,o=cern,o=ch"
                  passwd ,(password-store-get "icoteril"))))

(setq eudc-inline-query-format '((name)
                                 (firstname)
                                 (firstname name)))

