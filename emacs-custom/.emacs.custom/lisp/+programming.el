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
(add-hook 'after-init-hook #'global-company-mode)

;;; spelling
;; ispell
;; spell-fu
;; (use-package flyspell)
;; (use-package flyspell-correct-ivy)
;; (use-package flyspell-correct-popup)

;;; Grammar
;; TODO langtool
;; TODO writegood-mode

;;;; Code editing

;; electric
;; smartparens
;; (use-package smartparents
;;  :defer t)

;;; Platforms

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

;; paredit/lispy

;; common-lisp
(use-package sly
  :defer t)
(setq inferior-lisp-program "sbcl")
(setq sly-command-switch-to-existing-lisp t)

;; clojure
(use-package cider
  :defer t)

;; scheme
(use-package geiser
  :defer t)
(use-package geiser-guile
  :after geiser
  :defer t)
(use-package geiser-racket
  :after geiser
  :defer t)

(setq geiser-active-implementations '(guile))

;; racket
(use-package racket-mode
  :defer t)


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
