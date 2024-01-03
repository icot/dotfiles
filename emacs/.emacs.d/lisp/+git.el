;;; magit: Custom functions taken from https://github.com/nbarrientos/dotfiles/blob/master/.emacs.d/init.el#L1770

(use-package magit
  :defer t
  :config
  (add-to-list 'magit-clone-name-alist '("\\(it-puppet-.+\\)" "git@gitlab.cern.ch:7999" "ai"))
  (add-to-list 'magit-clone-name-alist '("\\`\\(?:cgl:\\)\\([^:]+\\)\\'" "gitlab.cern.ch:7999" "ai"))
  :custom
  (magit-clone-default-directory "~/workspace/puppet/"))

;;(use-package forge
;;  :after magit
;;  :config
;;  (add-to-list 'forge-alist
;;               '("gitlab.cern.ch" "gitlab.cern.ch/api/v4" "gitlab.cern.ch" 'forge-gitlab-repository)))
  
(use-package git-link
  :custom
  (git-link-use-commit t))

;;(use-package lab
;;  :ensure t
;;  :after password-store
;;  :straight (:host github :repo "isamert/lab.el")
;;  :config  (setq lab-host "https://gitlab.cern.ch"
;;                 lab-token (password-store-get "gitlab/magit-forge")
;;                 lab-group "db"
;;                 lab-projects-directory "~/workspace"))

;;(defun icot/project-pipelines ()
;;  (interactive)
;;  (lab-get-project-pipelines lab-project-id))

(defun icot/clone-module (module-name)
 "Clone a Puppet module from gitlab.cern.ch/ai"
 (interactive "sModule name: ")
 (let ((magit-clone-url-format "ssh://%h/%n.git")
       (magit-clone-set-remote.pushDefault t)
       (repo-name (concat "it-puppet-module-" module-name)))
   (magit-clone-internal
    ;; Using an internal here, see  https://github.com/magit/magit/discussions/4335
    (magit-clone--name-to-url repo-name)
    (concat magit-clone-default-directory repo-name)
    nil)))

(defun icot/clone-hostgroup (hostgroup-name)
  "Clone a Puppet top-level hostgroup from gitlab.cern.ch/ai"
  (interactive "sTop-level hostgroup name: ")
  (let ((magit-clone-url-format "ssh://%h/%n.git")
        (magit-clone-set-remote.pushDefault t)
        (repo-name (concat "it-puppet-hostgroup-" hostgroup-name)))
    (magit-clone-internal
     (magit-clone--name-to-url repo-name)
     (concat magit-clone-default-directory repo-name)
     nil)))
