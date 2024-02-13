

(use-package org-jira
  :after password-store
  :config
  (setq org-jira-working-dir "~/Sync/org-jira")
  (setq jiralib-url "https://its.cern.ch/jira")
  (setq jiralib-token `("Authorization" . ,(format "Bearer %s" (password-store-get "jira/jirauth"))))
  (setq org-jira-projects-list '("NILE" "Monitoring" "SWAN")))


(setq org-jira-custom-jqls
  '(
    (:jql "project = Monitoring and sprint in opensprints()"
          :filename "Monitoring-Sprint")
    (:jql "project = Monitoring and sprint not in opensprints()"
          :filename "Monitoring-Backlog")
    (:jql "project = NILE and sprint in opensprints()"
          :filename "NILE-Sprint")
    (:jql "project = NILE and sprint not in opensprints()"
              :filename "NILE-Backlog")
    (:jql "project = SWAN and sprint in opensprints()"
          :filename "SWAN-Sprint")
    (:jql "project = SWAN and sprint not in opensprints()"
          :filename "SWAN-Backlog")))

