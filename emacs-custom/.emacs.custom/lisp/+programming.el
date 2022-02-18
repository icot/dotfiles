;;; +programming.el --- Description -*- lexical-binding: t; -*-

;;;; File Templates

;;; file-templates/snippets
;; (use-package yasnippet)


;;;; Completion
(use-package company)
(add-hook 'after-init-hook #'global-company-mode)
 
;;;; linting

;;; syntax
(use-package flycheck)
(use-package flycheck-popup-tip)
(use-package flycheck-posframe)

;;; spelling
;; ispell
;; spell-fu
(use-package flyspell
  :defer t)
(use-package flyspell-correct-ivy
  :after (flyspell ivy))
(use-package flyspell-correct-popup
  :after (flyspell ivy))

;;; Grammar
;; TODO writegood-mode

;;;; Code editing

;; smartparens
;;(use-package smartparents
;;  :defer t)

;;; Platforms

;; guix
(use-package guix
  :defer t)

;; docker
(use-package docker
  :defer t)
(use-package dockerfile-mode
  :defer t)

;; terraform
(use-package terraform-mode
  :defer t)
(use-package company-terraform
  :after terraform-mode)

;; rest
(use-package restclient
  :defer t)
(use-package company-restclient
  :defer t)

;;;; Languages//programming

;;; LISPs

(use-package lispy
  :defer t)

;; Parinfer (archived)
;; https://github.com/DogLooksGood/parinfer-mode 

;(use-package parinfer
;  :defer t
;  :bind
;  (("C-," . parinfer-toggle-mode))
;  :init
;  (progn
;    (setq parinfer-extensions
;          '(defaults       ; should be included.
;            pretty-parens  ; different paren styles for different modes.
;            evil           ; If you use Evil.
;            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;            paredit        ; Introduce some paredit commands.
;            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;            smart-yank   ; Yank behavior depend on mode.
;    (add-hook 'clojure-mode-hook #'parinfer-mode)
;    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;    (add-hook 'scheme-mode-hook #'parinfer-mode)
;    (add-hook 'lisp-mode-hook #'parinfer-mode))


;; https://github.com/justinbarclay/parinfer-rust-mode
;; Considered alpha state
(use-package parinfer-rust-mode
    :hook emacs-lisp-mode
    :init
    (setq parinfer-rust-auto-download t))

;; common-lisp
(use-package sly
  :defer t
  :bind (:map lisp-mode-map
         ("C-c C-r" . sly-eval-region)
         ("C-c C-l" . sly-eval-last-expression)
         ("C-c C-b" . sly-eval-buffer)
         ("C-c C-d" . sly-eval-defun)))
(setq inferior-lisp-program "sbcl")
(setq sly-command-switch-to-existing-lisp t)

;; Add slime too. Sly can't connect to nyxt's swank server at v2.2.4
;(use-package slime
;  :defer t)

;; Clojure
(use-package cider
  :defer t)

;; scheme
(use-package geiser
  :defer t
  :bind (:map geiser-mode-map
         ("C-c C-r" . geiser-eval-region)
         ("C-c C-l" . geiser-eval-last-sexp)
         ("C-c C-d" . geiser-eval-definition)
         ("C-c C-b" . geiser-eval-buffer)
         ("C-x C-e" . geiser-eval-last-sexp)))

(use-package geiser-guile
  :after geiser
  :defer t)
(use-package geiser-racket
  :after geiser
  :defer t)

(setq geiser-active-implementations '(guile))

;; racket
(use-package racket-mode
  :defer t
  :bind (:map racket-mode-map
         ("C-c C-r" . racket-send-region)
         ("C-c C-l" . racket-send-last-sexp)
         ("C-c C-d" . racket-send-definition)
         ("C-x C-e" . racket-eval-last-sexp)))

;;; data formats
;; json
(use-package json-mode
  :defer t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

;; yaml
(use-package yaml-mode
  :defer t)

;;; Other

;; puppet
(use-package puppet-mode
  :defer t)

;; c/c++

;; go
(use-package company-shell
  :defer t)

;; javascript

;; python

;; latex

;; shell
;; Config for sh-script
(use-package company-shell
  :defer t)

;; web
(use-package company-web
  :defer t)
(use-package counsel-css
  :defer t)
