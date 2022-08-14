;;; PDF configurations

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  :hook
  (pdf-view-mode . (lambda ()
                     (set (make-local-variable 'evil-normal-state-cursor) (list nil))))
  (pdf-annot-list-mode . 'hide-mode-line-mode))

(use-package saveplace-pdf-view
  :after pdf-view
  :config
  (save-place-mode t))
