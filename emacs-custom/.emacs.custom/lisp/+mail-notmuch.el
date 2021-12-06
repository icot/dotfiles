;; Notmuch configuration and customizations

;; References:
;; - https://notmuchmail.org/emacstips/
;; - https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs.org
;; - https://gist.github.com/adamrt/168dd81f2a1b6b363469

;;  Signature verificaiton frezee fix: disable-crl-checks on ./gnupg/gpgsm.conf

;; TODO: Override tags in notmuch-search-result-format?
;;   - Doesn't seem possible via customization. The item to be display
;;   is a plist (or spec list) with :tags propoerty

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

;; https://notmuchmail.org/emacstips/#index11h2
(setq message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header
      message-kill-buffer-on-exit t
      mail-user-agent 'gnus-user-agent
      sendmail-program "/usr/bin/msmtp")

(use-package notmuch
  :commands (notmuch-jump-search notmuch-hello)
  :config
  (setq
    ;; General UI
    notmuch-show-logo nil
    notmuch-column-control t
    notmuch-hello-auto-refresh t
    notmuch-hello-recent-searches-max 20
    notmuch-hello-thousands-separator ""
    notmuch-show-all-tags-list nil
    ;;; Search
    notmuch-search-oldest-first nil
    notmuch-saved-searches `(
	    (:name "inbox" :query "tag:inbox not tag:trash" :key "i" :sort-order newest-first)
	    (:name "recent" :query "tag:inbox date:15_days..  not tag:trash" :key "r" :sort-order newest-first)
	    (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
	    (:name "todo" :query "tag:todo not tag:archived" :key "t" :sort-order newest-first)
	    (:name "events" :query "attachment:ics not tag:trash" :key "e" :sort-order newest-first)
	    (:name "sent" :query "tag:sent" :key "s" :sort-order newest-first)
	    (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first)))
  ;; Separate the sections override so it takes our modified saved searches
  (setq notmuch-hello-sections '(
                          notmuch-hello-insert-saved-searches
                          notmuch-hello-insert-inbox ;; tags
                           ))
  (setq notmuch-search-result-format
        '(("date" . "%12s ")
          ("count" . "%-7s ")
          ("authors" . "%-30s ")
          ("subject" . "%-72s ")
          ("tags" . "(%s)"))))
