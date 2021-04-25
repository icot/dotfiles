;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(map! :leader :desc "Dired fuzzy search" :nv "d" #'dired)

(setq package-native-compile t)
(setq comp-deferred-compilation t)

;; Auto-compile
(setq load-prefer-newer t)
(require 'auto-compile)
(after! auto-compile
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ignacio Coterillo"
      user-mail-address "ignacio.coterillo@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-material)

(setq doom-theme 'doom-homage-white)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/myorg/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Load custom functions
(load! "+functions")

;; Elfeed configuration
(load! "+elfeed")
;;(map! :leader
      ;;(:prefix-map ("o" . "open")
       ;;(:prefix ("e" . "elfeed")
        ;;:desc "Elfeed" "e" #'elfeed)))

(map! :leader
       (:prefix ("o" . "open")
        :desc "Elfeed" "e" #'elfeed))

;; mu4e Configuration
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(after! mu4e
  (load! "+mail"))

;; org-static-blog configuration
(load! "+blog")

;;(map! :leader :desc "magit" "g s" #'magit)

;; Slime -- M-x package-install RET slime
;; (setq inferior-lisp-program "~/.guix-profile/bin/sbcl")
(setq inferior-lisp-program "sbcl")
(setq sly-command-switch-to-existing-lisp t)

;; Set Copy/Paste expedted behaviour
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; SLY Customization
;(setq sly-command-switch-to-existing-lisp t)

;; Set transparency percentage
(defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

(transparency 100)

;;
(setq org-startup-folded "fold")

;; Org-Jira
;; (use-package! org-jira
;;  :load-path "lisp/org-jira"
;;  :hook (prog-mode . org-mode))
;; (setq org-jira-cloud nil)
;; (setq jiralib-url "https://its.cern.ch/jira")

;; Use Clojure mode for Babashka scripts
(add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))

;; Fix Doom doctor early-init warnings
(add-to-list '+evil-collection-disabled-list 'ibuffer)
(add-to-list '+evil-collection-disabled-list 'rtags)
(add-to-list '+evil-collection-disabled-list 'bookmark)
(add-to-list '+evil-collection-disabled-list 'popup)
(add-to-list '+evil-collection-disabled-list 'arc-mode)
(add-to-list '+evil-collection-disabled-list 'xref)
(add-to-list '+evil-collection-disabled-list 'racer) ; Rust
(add-to-list '+evil-collection-disabled-list 'flycheck)
(add-to-list '+evil-collection-disabled-list 'go-mode)
(add-to-list '+evil-collection-disabled-list 'apropos)
(add-to-list '+evil-collection-disabled-list 'sly)

;;
(after! 'magit
  (push '("gitlab.cern.ch" "gitlab.cern.ch.ch/api/v4" "gitlab.cern.ch" forge-gitlab-repository) forge-alist))

;; Calendar
(defun my-open-calendar()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    (cfw:ical-create-source "Moon" "~/.calendars/Y2FsOi8vMC8yMjg.ics" "Blue"))))
