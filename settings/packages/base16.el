(use-package base16-theme
  :ensure t
  :config
  (setq base16-distinct-fringe-background nil)
  (load-theme 'base16-default-dark t))
(custom-theme-set-faces
 'base16-default-dark
 `(default ((t (:background "#000000"))))
 `(fringe ((t (:foreground "#383838" :background "#000000"))))
 `(line-number ((t (:foreground "#383838" :background "#000000"))))
 `(line-number-current-line ((t (:foreground "#585858" :background "#000000")))))
(setq evil-default-cursor        '(box "#d8d8d8")
      evil-insert-state-cursor   '((bar . 2) "#d8d8d8")
      evil-motion-state-cursor   '(box "d8d8d8")
      evil-normal-state-cursor   '(box "#d8d8d8")
      evil-replace-state-cursor  '(hbar "#d8d8d8")
      evil-visual-state-cursor   '(hollow "#d8d8d8"))

;; Mutiedit (mutil-cursor) highlight color
(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "#D96CDA")
  (set-face-foreground 'iedit-occurrence "#31303D"))


(provide 'base16)
