;;; ../.dotfiles/emacs/.doom.d/+irc.el -*- lexical-binding: t; -*-

;; IRC
;;  Mostly taken from:
;;  https://github.com/nbarrientos/dotfiles/blob/master/.emacs.d/init.el


(use-package erc
  :defer t
  :config
  (erc-spelling-mode)
  :custom
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 14)
  (erc-fill-column (- (/ (frame-width) 2) 3))
  (erc-hide-list '("PART" "QUIT"))
  (erc-auto-query 'bury)
  (erc-join-buffer 'bury)
  (erc-kill-server-buffer-on-quit t)
  (erc-kill-queries-on-quit t)
  (erc-kill-buffer-on-part t)
  (erc-disable-ctcp-replies t)
  (erc-prompt (lambda nil (format "%s>" (buffer-name))))
  (erc-part-reason (lambda (&optional s) ""))
  (erc-user-mode "+iRw")
  (erc-nick "spikot")
  (erc-server "irc.libera.chat"))

(require 'erc-networks)
(add-to-list 'erc-networks-alist '(Libera.Chat "libera.chat"))
(add-to-list 'erc-server-alist
	    '("Libera.Chat: Random server" Libera.Chat "irc.libera.chat" 6697))

(require 'erc-join)
(setq erc-autojoin-timing 'ident)
(setq erc-autojoin-channels-alist '(("libera.chat" "#x3")))

(require 'erc-track)
(dolist (msg '("JOIN" "PART" "QUIT" "MODE"))
  (add-to-list 'erc-track-exclude-types msg))

(setq erc-format-query-as-channel-p nil)
(setq erc-track-priority-faces-only 'all)
(setq erc-track-faces-priority-list
      '(erc-error-face
	erc-current-nick-face
	erc-keyword-face
	erc-nick-msg-face
	erc-direct-msg-face
	erc-notice-face
	erc-prompt-face))

(defvar icot/libera-spikot-password (password-store-get "liberachat/spikot"))

(require 'erc-services)
(erc-services-mode 1)
;; This will be part of Emacs28
(add-to-list 'erc-nickserv-alist
	    '(Libera.Chat
		"NickServ!NickServ@services.libera.chat"
		"This\\s-nickname\\s-is\\s-registered.\\s-Please\\s-choose"
		"NickServ"
		"IDENTIFY" nil nil
		"You\\s-are\\s-now\\s-identified\\s-for\\s-"))
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
`((Libera.Chat
    (("spikot" . ,icot/libera-spikot-password)))))

(defun icot/erc-identify ()
  (interactive)
  (erc-nickserv-identify (password-store-copy "liberachat/spikot")))

(defun icot/erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.libera.chat:6697")
      (erc-track-switch-buffer 1)
      (erc-tls :server "irc.libera.chat" :port 6697 :nick "spikot")))

(use-package emojify
  :defer t
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode) 
