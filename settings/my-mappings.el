;; ======================== My Mappings ========================

(evil-define-key 'normal 'global (kbd "j") 'evil-next-visual-line)
(evil-define-key 'normal 'global (kbd "k") 'evil-previous-visual-line)

(map! :n "Q" #'evil-prev-buffer)
(map! :n "W" #'evil-next-buffer)

(map! :ne "SPC z" #'writeroom-mode)
(map! :leader
      :desc "Clear search highlight"
      :n "nh" 'evil-ex-nohighlight)
(map! :map company-active-map "C-l" 'company-complete-selection)

;; company-tng (tab and go) allows you to use TAB to both select a
;; completion candidate from the list and to insert it into the
;; buffer... See https://github.com/company-mode/company-mode/pull/706
(map! :map company-active-map "RET" nil)
(add-to-list 'company-frontends 'company-tng-frontend)

;; Duplicate and comment line
;; (define-key evil-normal-state-map (kbd "SPC d") (kbd "gccyypgcc"))
(map! :leader
      :desc "Comment-duplicate line"
      :n "d" "gccyypgcc")

;; make sure that scroll down works as expected
(map! :n "C-d" #'scroll-up-command)

;; Map find file in project
(map! :n "C-p" nil
      :n "C-P" nil
      :n "C-p" #'projectile-find-file
      :n "C-S-p" #'projectile-find-file-in-known-projects)

;; flyspell
(define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
(define-key flyspell-mouse-map [mouse-3] #'undefined)

;; Implements something similar to vim-easyclip
(evil-define-operator evil-delete-without-register (beg end type yank-handler)
  (interactive "<R><y>")
  (evil-delete beg end type ?_ yank-handler))

(evil-define-operator evil-delete-line-without-register (beg end type yank-handler)
  :motion nil
  (interactive "<R><y>")
  (evil-delete-line beg end type ?_ yank-handler))

(evil-define-operator evil-change-without-register (beg end type yank-handler)
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler))

(evil-define-operator evil-change-line-without-register (beg end type yank-handler)
  :motion evil-end-of-line-or-visual-line
  (interactive "<R><y>")
  (evil-change-line beg end type ?_ yank-handler))

(map! :n "d" 'evil-delete-without-register)
(map! :v "d" 'evil-delete-without-register)
(map! :n "D" 'evil-delete-line-without-register)
(map! :n "c" 'evil-change-without-register)
(map! :v "c" 'evil-change-without-register)
(map! :n "C" 'evil-change-line-without-register)
(map! :n "m" 'evil-delete)
(map! :v "m" 'evil-delete)
(map! :n "M" 'evil-delete-line)
(map! :mode evil-snipe-mode :n "S" nil)
(map! :n "S" 'evil-change)

(evil-define-operator evil-org-delete-without-register (beg end type register yank-handler)
  "Like evil-delete, but realigns tags and numbered lists."
  (interactive "<R><y>")
  (let ((renumber-lists-p (or (< beg (line-beginning-position))
                              (> end (line-end-position)))))
    (evil-delete beg end type ?_ yank-handler)
    (cond ((and renumber-lists-p (org-at-item-p))
           (org-list-repair))
          ((org-at-heading-p)
           (org-fix-tags-on-the-fly)))))

(defun org_mode_delete()
  (map! :map evil-org-mode-map :n "d" 'evil-org-delete-without-register)
  (map! :map evil-org-mode-map :v "d" 'evil-org-delete-without-register))

(add-hook! 'evil-org-mode-hook 'org_mode_delete)

;; export package
(provide 'my-mappings)
