;; Notmuch configuration and customizations

;; References:
;; - https://notmuchmail.org/emacstips/
;; - https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs.org
;; - https://gist.github.com/adamrt/168dd81f2a1b6b363469

;;  Signature verificaiton frezee fix: disable-crl-checks on ./gnupg/gpgsm.conf

;; TODO: Override tags in notmuch-search-result-format?
;;   - Doesn't seem possible via customization. The item to be display
;;   is a plist (or spec list) with :tags propoerty

(defun my-set-msmtp-account ()
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

(add-hook 'message-send-mail-hook 'my-set-msmtp-account)

(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-envelope-from 'header
      message-kill-buffer-on-exit t
      mail-user-agent 'mu4e-user-agent
      sendmail-program "/usr/bin/msmtp")

(use-package notmuch
  :defer t
  :config
  (setq
        ;; Syncing (manage externally)
        +notmuch-sync-backend 'custom
        +notmuch-get-sync-command "true"
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
                (:name "inbox" :query "tag:inbox not tag:trash" :key "i")
                (:name "recent" :query "tag:inbox date:15_days..  not tag:trash" :key "r")
                (:name "flagged" :query "tag:flagged" :key "f")
                (:name "todo" :query "tag:todo not tag:archived" :key "t")
                (:name "events" :query "attachment:ics not tag:trash" :key "e")
                (:name "sent" :query "tag:sent" :key "s")
                (:name "drafts" :query "tag:draft" :key "d")))
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
          ("tags" . "(%s)")))
  ;; Override home function
  ;; TODO: something off in switch-back
  (setq +notmuch-home-function (lambda () (notmuch-search "date:15_days.."))))

