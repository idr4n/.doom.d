(evil-define-operator evil-org-delete-without-register (beg end type yank-handler)
  (interactive "<R><y>")
  (evil-delete beg end type ?_ yank-handler))

(map! :n "d" 'evil-org-delete-without-register)
(map! :v "d" 'evil-org-delete-without-register)


;; export package
(provide 'mappings-org-mode)
