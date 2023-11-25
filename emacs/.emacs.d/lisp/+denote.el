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



