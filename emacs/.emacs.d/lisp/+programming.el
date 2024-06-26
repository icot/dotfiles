;;; +programming.el --- Description -*- lexical-binding: t; -*-

;;;; File Templates

;;; file-templates/snippets
;; (use-package yasnippet)

;; tree-sitter
;; (use-package tree-sitter-langs
;;    :ensure t)

(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  :hook (prog-mode . turn-on-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))
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

;;(use-package parinfer-rust-mode
;;  :hook (emacs-lisp-mode clojure-mode common-lisp scheme-mode lisp-mode)
;;  :config
;;  (setq parinfer-rust-auto-download t))

;; LSP Mode

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :commands lsp-uid-mode
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbols)

;; Servers

;; Python
;;; Pyright https://emacs-lsp.github.io/lsp-pyright/

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; Java
(use-package lsp-java
  :ensure t
  :defer t
  :hook (java-mode . lsp))

;; Clojure - Automatic install (lsp-install-server)

;; Rust - rls/rust analyzer
;; rust-analyzer installed via cargo
(use-package rust-mode
  :ensure t
  :defer t
  :hook (rust-mode . lsp))

;; Go
;;; gopls https://github.com/golang/tools/tree/master/gopls


;; Haskell
;;; https://emacs-lsp.github.io/lsp-haskell/

;; Bash - npm
;; Dockerfiles - npm
;; Elm -npm
;; Json - npm

;; Racket
;;  raco pkg install racket-langserver


;; Terraform - GO install


;; Debugger
;(use-package dap-mode)
;(use-package dap-LANG)


;; Considered alpha state
;;(use-package parinfer-rust-mode
;;    :hook emacs-lisp-mode
;;    :init
;;    (setq parinfer-rust-auto-download t))

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

(add-to-list 'exec-path "~/.sdkman/candidates/leiningen/current/bin")
(add-to-list 'exec-path "~/.asdf/shims/")

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
  :mode "\\.rkt\\'"
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

;;; golang

(use-package go-mode
  :defer t
  :ensure t)

;; https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
;; go install golang.org/x/tools/gopls@latest

(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(use-package haskell-mode
  :ensure t)

;; c/c++

;;(use-package company-shell
;;  :defer t)

;; javascript

;; python

;; latex

;; shell
;; Config for sh-script
;;(use-package company-shell
;;  :defer t

;; web
;;(use-package company-web
;;  :defer t)
;;(use-package counsel-css
;;  :defer t)
