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
  (setq org-directory "~/Sync/myorg/")
  (setq org-agenda-files `(,org-directory))
  (setq org-startup-with-inline-images t)
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "DONE")))
  (setq org-agenda-span 'day)
  (setq
   org-default-notes-file (expand-file-name +org-capture-notes-file org-directory)
   +org-capture-journal-file (expand-file-name +org-capture-journal-file org-directory)
   +org-capture-todo-file (expand-file-name +org-capture-todo-file org-directory)
   +org-capture-notes-file (expand-file-name +org-capture-notes-file org-directory)))

;  :hook (org-mode . icot/org-add-electric-pairs)

;; Org Modern https://github.com/minad/org-modern

(use-package org-modern
  :ensure t)
(global-org-modern-mode)

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-directory org-directory))

(use-package org-noter
  :ensure t
  :after org
  :config
  (setq org-noter-default-notes-file-names +org-capture-notes-file))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(require 'org-noter-pdftools)

(defun icot/mail-todo-format-string ()
  "Create todo format string from mail contents. Assumes notmuch use"
  (let ((from (notmuch-show-get-from))
        (subject (notmuch-show-get-subject)))
    (format "* [ ] %%u %s %s %%?" from subject)))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?\n%i\n%a" :prepend t)
        ("m" "Mail Todo" entry (file+headline +org-capture-todo-file "Inbox")
         (function icot/mail-todo-format-string) :prepend t)
        ("n" "Notes" entry (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :prepend t)))

;; org-tree-slide
