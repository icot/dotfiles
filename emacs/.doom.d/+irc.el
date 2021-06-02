;;; ../.dotfiles/emacs/.doom.d/+irc.el -*- lexical-binding: t; -*-

;; IRC
(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick "spikot"
      :sasl-username "spikot"
      :sasl-password (lambda (&rest _) (+pass-get-secret "liberachat/spikot"))
      :channels ("#guix"))))

(setq erc-server "irc.libera.chat"
      erc-nick "spikot"
      erc-prompt-for-password nil
      erc-autojoin-channels-alist '(("x3"))
      erc-kill-buffer-on-part t
      erc-auto-query 'bury)

(require 'erc-services)
(erc-services-mode 1)
(erc-update-modules)

(setq erc-nickserv-identify-mode 'autodetect
      erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords `((liberachat
                                (("spikot" . ,(+pass-get-secret "liberachat/spikot"))))))

(add-to-list 'erc-nickserv-alist
             '(liberachat
               "NickServ@services.libera.chat"
               "This nickname is registered. Please choose a different nickname,"
               "NickServ"     ; Identify to
               "IDENTIFY"     ; Identify keyword
               nil )) ; Use current nick

(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 22)

(defun my/erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "irc.libera.chat:6697")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc-tls :server "irc.libera.chat" :port 6697 :nick "spikot"))))

(defun my/erc-identify ()
  (erc-nickserv-identify (+pass-get-secret "liberachat/spikot")))
