#!guile \
-e main -s
!#

;;; Commentary:
;;
;; Usage: dostow FOLDER
;;
;; This tool is a GNU Guile implementation of part of the functionality of
;; the GNU Stow utility, dependint strongly on certain assumptions (see list),
;; while fixing stow issues with "dot-" syntax for folders, and adding support
;; for basic templating to address some limitations I found when deploying
;; certain of my dot files on different systems with different usernames, etc.
;; 
;; Assumptions:
;;   - FOLDER is located in ${HOME}/<whatever-name-you-use>/FOLDER
;;   - files to be ignored are suffixed with '-nostow' or '-NOSTOW'
;;   - dot files or folder names are preffixed by 'dot-'
;;   - template file names are preffixed by 'template-'
;;      - templates are evaluable Guile scripts returning the contents to be
;;        written to the intended file (OBVIOUSLY UNSAFE)  

;;; Code:

(use-modules
 (ice-9 format)
 (ice-9 ftw)
 (ice-9 regex)
 (srfi srfi-1))

;; Assumption: We always deploy on $HOME as base
(define base-path (getenv "HOME"))

(define (target-name filename)
  "Returns intended resulting filename"
  (let ((m (string-match "^(dot|template)-" filename)))
    (if m
	(cond
	 ((equal? (match:substring m) "dot-")
	  (format #f ".~a" (match:suffix m)))
	 ((equal? (match:substring m) "template-")
	  (format #f "~a" (match:suffix m))))
	filename)))

(define (target-path path)
  "Returns intended resulting path"
  path)

(define (process-template src dst)
  "Evaluate template located at <arg:src> and write output to <arg:dst>"
  (begin
    (format #t "Template: evaluate ~a on ~a ~%" src dst)
    (let ((content (load src)))
      (call-with-output-file dst
	(lambda (port)
	  (display content port))))))
    
(define (process-folder name result)
  "Process folder"
  (let* ((target (map target-name (string-split name #\/)))
	 (dname (string-join (cons base-path target) "/")))
    (format #t "[Nesting: ~a] Folder ~a -> ~a~%" result name dname)
    (if (not (file-exists? dname))
	(begin
	  (format #t "Creating folder: ~a ~%" dname)
	  (mkdir dname)))
    (+ result 1)))

(define (process-file name result)
  "Process standard files"
  (let* ((bname (basename name))
	 (target (map target-name (string-split name #\/)))
	 (source (string-join (list (getenv "PWD") name) "/"))
	 (fname (string-join (cons base-path target) "/")))
    (if (or
	 (string-match "-NOSTOW$" bname)
	 (string-match "-nostow$" bname))
	(begin
	  (format #t "Skipping file ~a~%" bname)
	  result)
	(begin
	  (format #t "Processing file ~a with target name ~a~%" source fname)
	  (let ((m (string-match "^template-" bname)))
	    (if m
		;; Evaluate template
		(process-template source fname)
	        (begin
		  (delete-file fname)
		  (symlink source fname))))
	  result))))

;; https://www.gnu.org/software/guile/manual/html_node/File-Tree-Walk.html
(define (dotstow directory-name)
  "stow files under FOLDER (similar to GNU Stow.)"

  (define (enter? name stat result)
    ;; Skip version control and not stowed directories"
    (not (or
	  (member (basename name) '(".git" "ignore"))
	  (string-match "-NOSTOW$" (basename name)))))
  
  (define (leaf name stat result)
    ;; Process files
    (process-file name result))

  ;; Process directories
  (define (down name stat result)
    (process-folder name result))
  
  (define (up name stat result)
    (format #t "Exit folder: ~a~%" name)
    (- result 1))

  ;; Likewise for skipped directories.
  (define (skip name stat result)
    (format #t "Skipping folder: ~a~%" name)
    result)

  ;; Ignore unreadable files/directories but warn the user.
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)

  (file-system-fold enter? leaf down up skip error
                           0  ; initial counter is zero processed files
                           directory-name))

;; Entry point
(define (main args)
  (if (= (length args) 2)
      (dotstow (cadr args))
      (format #t "Usage: mystow FOLDER~%")))

(provide 'dotstow)

;;; mystow ends here
