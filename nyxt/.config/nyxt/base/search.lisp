(in-package #:nyxt-user)

;;search engines setup | last one in list becomes default
;;order: "shortcut" "url for search, ~a determines where query is placed" "fallback url when query fails or an empty search"
(defvar *my-search-engines*
  (list
   '("git" "https://github.com/search?q=~a" "https://github.com/") ;;git repos
   '("google" "https:/www.google.com/search?q=" "https://google.com")) ;; Google
   "List of search engines")

(define-configuration buffer
  ((search-engines (append (mapcar (lambda (engine) (apply 'make-search-engine engine))
                                   *my-search-engines*)
                           %slot-default%))))
