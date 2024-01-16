;;; Denote

(use-package denote
  :defer t
  :straight (:host nil
                   :repo "https://git.sr.ht/~protesilaos/denote")
  :hook (dired-mode . denote-dired-mode-in-directories)
  :init
  (setq denote-directory (expand-file-name "~/Sync/denote/"))
  :config
  (setq denote-known-keywords '("emacs" "management" "monit" "nile" "lisp"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; Org is the default, set others here
  (setq denote-prompts '(title keywords))
  (setq denote-link-fontify-backlinks t))

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d ,denote-directory)))
  :bind
  (("C-n o" . consult-notes)
   ("C-n s" . consult-notes-search-in-all-notes)))

(defun icot/my-denote-journal ()
  "Create an entry tagged 'journal', while prompting for a title."
  (interactive)
  (denote
   (denote--title-prompt)
   '("journal")))

(global-set-key (kbd "C-n") nil) ; Override next-line binding to free prefix
(global-set-key (kbd "C-n n") #'denote)
(global-set-key (kbd "C-n s") #'denote-subdirectory)
(global-set-key (kbd "C-n i") #'denote-link)
(global-set-key (kbd "C-n I") #'denote-link-add-links)
(global-set-key (kbd "C-n l") #'denote-link-find-file)
(global-set-key (kbd "C-n b") #'denote-link-backlinks)
(global-set-key (kbd "C-n d") #'(lambda () (interactive) (dired-other-window denote-directory)))

(evil-define-key 'normal 'global (kbd "<leader>nn") #'denote)
(evil-define-key 'normal 'global (kbd "<leader>ns") #'denote-subdirectory)
(evil-define-key 'normal 'global (kbd "<leader>ni") #'denote-link)
(evil-define-key 'normal 'global (kbd "<leader>nI") #'denote-link-add-links)
(evil-define-key 'normal 'global (kbd "<leader>nl") #'denote-link-find-file)
(evil-define-key 'normal 'global (kbd "<leader>nb") #'denote-link-backlinks)
(evil-define-key 'normal 'global (kbd "<leader>no") #'consult-notes)
(evil-define-key 'normal 'global (kbd "<leader>ns") #'consult-notes-search-in-all-notes)
(evil-define-key 'normal 'global (kbd "<leader>nd") #'(lambda () (interactive) (dired-other-window denote-directory)))

