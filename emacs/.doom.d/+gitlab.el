;;; functions.el -*- lexical-binding: t; -*-

;; Playground

;; From https://github.com/nbarrientos/dotfiles/blob/master/.emacs.d/init.el
;;(use-package magit
;;  :config
(after! 'magit
  (push '("gitlab.cern.ch" "gitlab.cern.ch.ch/api/v4" "gitlab.cern.ch" forge-gitlab-repository) forge-alist)
  (add-to-list 'magit-clone-name-alist '("\\(it-puppet-.+\\)" ":@gitlab.cern.ch:8443" "ai"))
  (transient-append-suffix 'magit-push "-n"
    '(1 "-M" "Create MR in Gitlab" "--push-option=merge_request.create"))
  (setq magit-save-repository-buffers 'dontask
        magit-clone-default-directory "~/dev/"
        magit-clone-url-format "https://%h/%n.git"
        magit-clone-set-remote.pushDefault t))

(defun my/clone-module (module-name)
  "Clone a Puppet module from gitlab.cern.ch/ai"
  (interactive "sModule name: ")
  (magit-clone-internal
   ;; Using an internal here, see  https://github.com/magit/magit/discussions/4335
   (magit-clone--name-to-url (concat "it-puppet-module-" module-name))
   magit-clone-default-directory
   nil))

(defun my/clone-hostgroup (hostgroup-name)
  "Clone a Puppet top-level hostgroup from gitlab.cern.ch/ai"
  (interactive "sTop-level hostgroup name: ")
  (magit-clone-internal
   (magit-clone--name-to-url (concat "it-puppet-hostgroup-" hostgroup-name))
   magit-clone-default-directory
   nil))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun my/find-url (path &optional depth)
  (if (> depth 10) (message ".git not found")
    (let ((dfs (directory-files path)))
      (if (member ".git" dfs)
        (substring
         (cadr
          (split-string
           (cadr
            (split-string
             (car (seq-filter (lambda (s) (string-match "url" s)) (read-lines (format "%s/.git/config" path))))
             "=" t))
           ":" t))
         0 -4)
        (find-url (concat path "../") (1+ depth))))))

(defun my/gitlab-ci ()
  (interactive)
  (message (my/find-url (file-name-directory buffer-file-name) 0)))


;;(let ((remote (car (magit-git-lines "remote" "-v" "show"))))
;;  (save-match-data
;;    (and (string-match "\\([^@]+\\):\\([^@]+\\)/\\([^@]+\\).git" remote)
;;         (setq server (match-string 1 remote)
;;               groups (match-string 2 remote)
;;               project (match-string 3 remote)))))
