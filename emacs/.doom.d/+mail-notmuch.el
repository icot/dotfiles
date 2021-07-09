;; Notmuch configuration and customizations

;; References:
;; - https://notmuchmail.org/emacstips/
;; - https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs.org
;; - https://gist.github.com/adamrt/168dd81f2a1b6b363469

;;  Signature verificaiton frezee fix: disable-crl-checks on ./gnupg/gpgsm.conf

;; TODO: Override tags in notmuch-search-result-format?
;;   - Doesn't seem possible via customization. The item to be display
;;   is a plist (or spec list) with :tags propoerty

(after! notmuch
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
  (setq +notmuch-home-function (lambda () (notmuch-search "date:15_days..")))
  ;; Undo pop up rule from doom module for notmuch-hello
  ;;   https://github.com/hlissner/doom-emacs/issues/4585
  (after! notmuch (set-popup-rule! "^\\*notmuch-hello" :ignore t)))

;; Global keybindings
(map! :leader :desc "Notmuch: jump to saved" :nv "mj" #'notmuch-jump-search)
(map! :leader :desc "Notmuch: search-by-tag" :nv "mt" #'notmuch-search-by-tag)
(map! :leader :desc "Notmuch: custom search" :nv "ms" #'notmuch-search)
;; TODO: forward message keybinding
