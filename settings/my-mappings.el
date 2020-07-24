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

;; redifen evil-delete
(evil-define-operator evil-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type ?_ yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (called-interactively-p 'any)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evil-delete-with-register (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (let ((evil-was-yanked-without-register nil))
    (evil-yank beg end type register yank-handler))
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'delete-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (delete-region (1- beg) end))
   (t
    (delete-region beg end)))
  ;; place cursor on beginning of line
  (when (and (called-interactively-p 'any)
             (eq type 'line))
    (evil-first-non-blank)))

(evil-define-operator evil-delete-line-without-register (beg end type yank-handler)
  :motion nil
  (interactive "<R><y>")
  (evil-delete-line beg end type ?_ yank-handler))

(evil-define-operator evil-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (let ((delete-func (or delete-func #'evil-delete))
        (nlines (1+ (evil-count-lines beg end)))
        (opoint (save-excursion
                  (goto-char beg)
                  (line-beginning-position))))
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (funcall delete-func beg end type ?_ yank-handler)
    (cond
     ((eq type 'line)
      (if ( = opoint (point))
          (evil-open-above 1)
        (evil-open-below 1)))
     ((eq type 'block)
      (evil-insert 1 nlines))
     (t
      (evil-insert 1)))))

(evil-define-operator evil-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion evil-end-of-line-or-visual-line
  (interactive "<R><x><y>")
  (evil-change beg end type ?_ yank-handler #'evil-delete-line))

(evil-define-operator evil-delete-char-without-register (beg end type register)
  "Delete next character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-delete beg end type ?_ register))

(map! :n "D" 'evil-delete-line-without-register)
(map! :n "m" 'evil-delete-with-register)
(map! :v "m" 'evil-delete-with-register)
(map! :n "M" 'evil-delete-line)
(map! :n "x" 'evil-delete-char-without-register)
(map! :mode evil-snipe-mode :n "S" nil)
(map! :n "S" 'evil-change)

(evil-define-operator evil-org-delete-char-without-register (count beg end type register)
  "Combine evil-delete-char with org-delete-char"
  :motion evil-forward-char
  (interactive "p<R><x>")
  (if (evil-visual-state-p)             ; No special support for visual state
      (evil-delete-char-without-register beg end type register)
    (evil-set-register ?- (filter-buffer-substring beg end))
    (evil-yank beg end type ?_ register)
    (org-delete-char count)))

(defun org_mode_delete()
  (map! :map evil-org-mode-map :n "x" 'evil-org-delete-char-without-register)
  (map! :map evil-org-mode-map :v "x" 'evil-org-delete-char-without-register))

(add-hook! 'evil-org-mode-hook 'org_mode_delete)

;; export package
(provide 'my-mappings)
