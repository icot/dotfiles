;;; ../.dotfiles/emacs/.doom.d/+exwm.el -*- lexical-binding: t; -*-

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

;;(require 'exwm-randr)
;;(add-hook 'exwm-randr-screen-change-hook
;;          (lambda ()
;;            (start-process-shell-command
;;             "xrandr" nil "xrandr ...")))

(setq exwm-workspace-number 10
      exwm-input-prefix-keys '(?\M-x
                               ?\M-:)
;;      exwm-simulation ;; rebind
      exwm-input-global-keys '(([s-return . eshell])))
