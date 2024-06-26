;; Notmuch configuration and customizations

;; References:
;; - https://notmuchmail.org/emacstips/
;; - https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs.org
;; - https://gist.github.com/adamrt/168dd81f2a1b6b363469

;;  Signature verificaiton frezee fix: disable-crl-checks on ./gnupg/gpgsm.conf

;; TODO: Override tags in notmuch-search-result-format?
;;   - Doesn't seem possible via customization. The item to be display
;;   is a plist (or spec list) with :tags propoerty

(use-package notmuch
  :ensure t
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
	    (:name "inbox" :query "tag:inbox not tag:trash" :key "i" :sort-order newest-first :search-type tree)
	    (:name "recent" :query "date:15_days.. and not tag:trash and not tag:snow and not tag:jira and not tag:lists" :key "r" :sort-order newest-first :search-type tree)
	    (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first :type tree)
	    (:name "snow" :query "tag:snow" :key "S" :sort-order newest-first :type tree)
	    (:name "monit" :query "tag:lists/monit-support" :key "M" :sort-order newest-first :type tree)
	    (:name "jira" :query "tag:jira" :key "j" :sort-order newest-first :type tree)
	    (:name "todo" :query "tag:todo not tag:archived" :key "t" :sort-order newest-first :search-type tree)
	    (:name "events" :query "attachment:ics not tag:trash" :key "e" :sort-order newest-first :search-type tree)
	    (:name "sent" :query "tag:sent" :key "s" :sort-order newest-first :search-type tree)
	    (:name "drafts" :query "tag:draft" :key "d" :sort-order newest-first :search-type tree)))
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

;; From https://notmuchmail.org/emacstips/
;; Modification of user/mm-pipe-- and user/notmuch-show-pop-attachment-to-buffer

(require 'cl-lib)
(cl-loop for p in load-path
      do (if (file-accessible-directory-p p)
             (let ((m (directory-files-recursively p "^ol-notmuch.el$")))
                  (if m (add-to-list 'load-path (file-name-directory (car m)))))))

;; Required by ol-notmuch via require
(use-package compat
  :ensure t)

(use-package ol-notmuch
  :ensure t
  :after 'compat
  :bind ("C-c t" . org-store-link)) ; Incompatible with LSP?



(defun icot/mm-pipe-- (handle cmd)
  ;; conveniently, '-' '-' a args to pdftotext and docx2txt.pl work fine
  ;; fixme: naming inconsistency (fn name and buffer name)
  (let ((buffer (get-buffer-create "*attachment-to-text*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-temp-buffer
      ;; "based on mm-pipe-part in mm-decode.el"
      (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (mm-add-meta-html-tag handle)
    (let ((coding-system-for-write 'binary))
      (call-process-region (point-min) (point-max)
                           cmd nil buffer nil "-" "-"))))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (text-mode)
    (visual-line-mode)
    (view-mode)))

(defun icot/process-ics-event (handle)
  "Process application/ics type MIME parts"
  (let ((buffer (get-buffer-create "*processing-ics-event*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (mm-insert-part handle)
      (icalendar-import-buffer))))

(defun icot/notmuch-show-process-attachment ()
  ;; "based on notmuch-show-apply-to-current-part-handle"
  (interactive)
  (let ((handle (notmuch-show-current-part-handle)))
    ;;(message "%s" handle)
    (unwind-protect
    (pcase (car (nth 1 handle))
      ("application/pdf"
       (icot/mm-pipe-- handle "pdftotext"))
      ("application/ics"
       (icot/process-ics-event handle))
      (_ (notmuch-show-save-part)))
    (kill-buffer (mm-handle-buffer handle)))))


;; helpers to toogle flagged and deleted tags
(defun icot/notmuch-tree-toggle-tag (tag)
  (let* ((tags (notmuch-tree-get-tags))
         (new-tags (list (concat (if (member tag tags) "-" "+") tag)))) 
;;    (message "tags-raw: %s" tags)
;;    (message "new-tags: %s" new-tags)
    (notmuch-tag (notmuch-tree-get-message-id) new-tags)
    (notmuch-tree-tag-update-display new-tags)))

(defun icot/notmuch-tree-toggle-tag-flagged ()
  (interactive)
  (icot/notmuch-tree-toggle-tag "flagged"))

(defun icot/notmuch-tree-toggle-tag-deleted ()
  (interactive)
  (icot/notmuch-tree-toggle-tag "deleted"))

(define-key notmuch-tree-mode-map (kbd "=") 'icot/notmuch-tree-toggle-tag-flagged)
(define-key notmuch-tree-mode-map (kbd "d") 'icot/notmuch-tree-toggle-tag-deleted)

(setq notmuch-show-part-button-default-action
      #'icot/notmuch-show-process-attachment)
