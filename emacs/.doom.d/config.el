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

;; Start server
;; (server-start)

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
(setq doom-font (font-spec :family "Dejavu Sans Mono" :size 16 :weight 'normal)
     doom-variable-pitch-font (font-spec :family "sans" :size 16))

;(set-face-attribute 'default nil :height 120)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-material)

(setq doom-theme (if (display-graphic-p) 'modus-operandi 'doom-1337))

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

;; Elfeed configuration
(load! "+elfeed")
;;(map! :leader
      ;;(:prefix-map ("o" . "open")
       ;;(:prefix ("e" . "elfeed")
        ;;:desc "Elfeed" "e" #'elfeed)))

(map! :leader
       (:prefix ("o" . "open")
        :desc "Elfeed" "e" #'elfeed))

;; Mail configuration

(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-envelope-from 'header
      message-kill-buffer-on-exit t
      sendmail-program "/usr/bin/msmtp")

(defun my-set-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
        (let*
            ((from (save-restriction
                     (message-narrow-to-headers)
                     (message-fetch-field "from")))
             (account
              (cond
               ((string-match "gmail.com" from) "gmail")
               ((string-match "cern.ch" from) "cern"))))
          (setq message-sendmail-extra-arguments (list '"-a" account))))))

(add-hook 'message-send-mail-hook 'my-set-msmtp-account)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(after! mu4e
  (load! "+mail-mu4e"))
(after! notmuch
  (load! "+mail-notmuch"))

;; org-static-blog configuration
(if (file-exists-p "./+blog.el")
    (load! "+blog"))

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

(transparency 99)

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
(add-to-list '+evil-collection-disabled-list 'ivy)
(add-to-list '+evil-collection-disabled-list 'dired)
(add-to-list '+evil-collection-disabled-list 'grep)
(add-to-list '+evil-collection-disabled-list 'compile)

;; Gitlab
(load! "+gitlab")

;; Calendar
(defun my-open-calendar()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    (cfw:ical-create-source "Moon" "~/.calendars/Y2FsOi8vMC8yMjg.ics" "Blue"))))


;; IRC
(load! "+irc")

;; MISC

;; Set Auth Source to use pass
;;   https://www.gnu.org/software/emacs/manual/html_mono/auth.html#Top
(setq auth-sources '(password-store))

(setq geiser-active-implementations '(guile))

;; eshell: binding + popup rule
(map! :leader :desc "eshell" :nv "ft" #'eshell)
(set-popup-rule! "^\\*eshell" :side 'right :size 0.33 :slot 1 :select t :quit nil :ttl 0)
(set-popup-rule! "^\\*vterm" :size 0.33 :vslot -4 :slot -4 :select t :quit nil :ttl 0)

;; Load EXWM when on aoi host
(if (string= "aoi" (system-name))
    (load! "+exwm"))

;; PDF recommendations
;; hlissner - https://www.reddit.com/r/emacs/comments/gshn9c/doom_emacs_as_a_pdf_viewer/
;;(use-package! pdf-view
  ;;:hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  ;;:hook (pdf-tools-enabled . hide-mode-line-mode)
  ;;:config
  ;;(setq pdf-view-midnight-colors '("#ABB2BF" . "#282C35")))

(map! :leader :desc "Dired fuzzy search" :nv "d" #'dired)
