;;; ../.dotfiles/emacs/.doom.d/+exwm.el -*- lexical-binding: t; -*-

;; Mix recommendations from DistroTube
;;  and https://github.com/nbarrientos/dotfiles/blob/master/.emacs.d/init.el

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
      ;; Always on bindings
      exwm-input-prefix-keys '(?\M-x
                               ?\M-:)
      ;; Rebind
      exwm-input-simulation-keys '(([?\C-a] . [home])
                                   ([?\C-e] . [end])
                                   ([?\C-d] . [delete])
                                   ([?\C-k] . [S-end delete]))
      ;; EXWM bindings
      exwm-input-global-keys '(([s-t] . eshell)
                               ([s-h] . split-window-below)
                               ([s-v] . split-window-right)
                               ([s-k] . delete-window)
                               ([s-b] . balance-windows)
                               ([s-p] .
                                     (lambda ()
                                       (interactive)
                                       (start-process "" nil "rofi -show run")))
                               ([s-r] . exwm-reset)))
