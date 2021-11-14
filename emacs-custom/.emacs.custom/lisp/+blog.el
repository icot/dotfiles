;;; +blog.el --- Description -*- lexical-binding: t; -*-

(use-package org-static-blog
  :defer t
  :straight
  (org-static-blog :type git
		   :host github
		   :repo "bastibe/org-static-blog"))

;; TODO Move following lines to :config

(setq org-static-blog-publish-title "icot.github.io"
      org-static-blog-publish-url "https://icot.github.io/"
      org-static-blog-publish-directory "~/workspace/org-blog/"
      org-static-blog-posts-directory "~/workspace/org-blog/posts/"
      org-static-blog-drafts-directory "~/workspace/org-blog/drafts/"
      org-static-blog-enable-tags t
      org-export-with-toc t
      org-static-blog-use-preview t
      org-static-blog-preview-convert-titles t
      org-static-blog-preview-ellipsis "...")

;; Tags from https://jao.io/blog/2020-02-11-simplicity.html
(setq icot-org-blog-tags
      (mapcar (lambda (f)
                (string-match "tag-\\(.+\\)\\.html" f)
                (format "<a href=\"/%s\">%s</a>"
                        f (match-string 1 f)))
              (directory-files "~/workspace/org-blog" nil "tag-.*")))

;; helper functions

(defun blog/read-static-html (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

;; Header
(setq org-static-blog-page-header
      (blog/read-static-html "~/workspace/org-blog/static/header.html"))

;; Preamble
(setq org-static-blog-page-preamble
       (concat
         "<div class=\"header\">"
         "  <a href=\"https://icot.github.io\">icot.github.io</a>"
         "  <div class=\"sitelinks\">"
         "    <a href=\"about.html\">about</a>"
         "    | <a href=\"archive.html\">archive</a>"
         "    | <div class=\"dropdown\">"
         "       <a href=\"tags/html\" class=\"dropbtn\">tags</a>"
         "       <div class=\"dropdown-content\">"
         (mapconcat #'identity icot-org-blog-tags "")
         "       </div>"
         "      </div>"
         "    | <a href=\"rss.xml\">rss</a>"
         "  </div>"
         "</div>"))

;; Postamble

(setq org-static-blog-page-postamble
      (blog/read-static-html "~/workspace/org-blog/static/postamble.html"))

;; Front Matter (inserted into the index page between the preamble and blog posts)
(setq org-static-blog-index-front-matter nil)
