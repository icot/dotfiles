;;; +org.el -*- lexical-binding: t; -*-

;; https://emacs.stackexchange.com/questions/10029/org-mode-how-to-create-an-org-mode-markup-keybinding
(defvar org-electric-pairs '((?\* . ?\*) (?/ . ?/) (?= . ?=)
                             (?\_ . ?\_) (?~ . ?~) (?+ . ?+)) "Electric pairs for org-mode.")

;; From Doom customizations
(defvar +org-capture-todo-file "todo.org"
  "Default target for todo entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-changelog-file "changelog.org"
  "Default target for changelog entries.

Is relative to `org-directory' unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-notes-file "notes.org"
  "Default target for storing notes.

Used as a fall back file for org-capture.el, for templates that do not specify a
target file.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-journal-file "journal.org"
  "Default target for storing timestamped journal entries.

Is relative to `org-directory', unless it is absolute. Is used in Doom's default
`org-capture-templates'.")

(defvar +org-capture-projects-file "projects.org"
  "Default, centralized target for org-capture templates.")

(defun icot/org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(use-package org
  :ensure t
  :straight (:type built-in)
  :init
  (setq org-directory "~/Nextcloud/myorg/")
  (setq org-agenda-files `(,org-directory))
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "DONE")))
  (setq org-agenda-span 'day)
  (setq
   org-default-notes-file (expand-file-name +org-capture-notes-file org-directory)
   +org-capture-journal-file (expand-file-name +org-capture-journal-file org-directory)
   +org-capture-todo-file (expand-file-name +org-capture-todo-file org-directory)
   +org-capture-notes-file (expand-file-name +org-capture-todo-file org-directory)))

;  :hook (org-mode . icot/org-add-electric-pairs)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-directory org-directory))

(setq org-capture-templates
    '(("t" "Todo" entry
	(file+headline +org-capture-todo-file "Inbox")
	"* [ ] %?\n%i\n%a" :prepend t)
	("n" "Notes" entry
	(file+headline +org-capture-notes-file "Inbox")
	"* %u %?\n%i\n%a" :prepend t)
	("j" "Journal" entry
	(file+olp+datetree +org-capture-journal-file)
	"* %U %?\n%i\n%a" :prepend t)))

;; org-noter
;; org-pdftools
;; org-tree-slide
