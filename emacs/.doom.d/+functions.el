;;; functions.el -*- lexical-binding: t; -*-

;; Playground

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
