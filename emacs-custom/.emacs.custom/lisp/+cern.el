;;; From https://github.com/nbarrientos/dotfiles/blob/276c6ba8402b447fadf7761895a256253e57313c/.emacs.d/init.el#L1677-L1726

(defun cern/ldap-group (arg group)
  "Print in buffer *LDAP GROUP* the members of GROUP.
With any prefix argument, make it not recursive."
  (interactive "P\nsGroup: ")
  (let ((buffer-n (format "*LDAP %s*" group))
        (members (cern/ldap-group-expand group (not arg))))
    (if members
        (with-temp-buffer-window
            buffer-n
            #'temp-buffer-show-function
            nil
          (dolist (member members)
            (princ (format "%s\n" member)))
          (with-current-buffer
              buffer-n
            (local-set-key (kbd "q") 'kill-this-buffer)
            (local-set-key (kbd "C-<return>") 'cern/ldap-user)
            (sort-lines nil (point-min) (point-max))))
      (message "%s" (propertize "Empty or unknown group!" 'face 'alert-high-face)))))

(defun cern/ldap-group-expand (group &optional recurse)
  "Return (recursively if RECURSE) the members of GROUP."
  (let ((ldap-host-parameters-alist
         (list
          (append
           (assoc "ldap://localhost:1389" ldap-host-parameters-alist)
           '(base "OU=e-groups,OU=Workgroups,DC=cern,DC=ch"))))
        (results nil))
    (dolist (member (car (ldap-search)
                     (format "(&(objectClass=group)(CN=%s))" group)
                     "ldap://localhost:1389"
                     '("member")))
      (and-let* ((dn (car (cdr member)))
                 (match (string-match "^CN=\\(.+?\\),OU=\\(.+?\\),OU=\\(.+?\\),DC=cern,DC=ch" dn))
                 (cn (match-string 1 dn))
                 (ou-1 (match-string 2 dn))
                 (ou-2 (match-string 3 dn)))
        (cond ((and
                recurse
                (string= "e-groups" (downcase ou-1)))
               (setq results (append (cern/ldap-group-expand cn recurse) results)))
              ((string= "users" (downcase ou-1))
               (push cn results))
              ((and
                (length= ou-1 1)
                (string= "externals" (downcase ou-2)))
               nil)
              (t
               (push dn results)))))
    (delete-dups results)))

(defun cern/clone-module (module-name)
  "Clone a Puppet module from gitlab.cern.ch/ai"
  (interactive "sModule name: ")
  (let ((magit-clone-url-format "https://%h/%n.git")
        (magit-clone-set-remote.pushDefault t)
        (repo-name (concat "it-puppet-module-" module-name)))
    (magit-clone-internal
     ;; Using an internal here, see  https://github.com/magit/magit/discussions/4335
     (magit-clone--name-to-url repo-name)
     (concat magit-clone-default-directory repo-name)
     nil)))

(defun cern/clone-hostgroup (hostgroup-name)
  "Clone a Puppet top-level hostgroup from gitlab.cern.ch/ai"
  (interactive "sTop-level hostgroup name: ")
  (let ((magit-clone-url-format "https://%h/%n.git")
        (magit-clone-set-remote.pushDefault t)
        (repo-name (concat "it-puppet-hostgroup-" hostgroup-name)))
    (magit-clone-internal
     (magit-clone--name-to-url repo-name)
     (concat magit-clone-default-directory repo-name)
     nil)))
