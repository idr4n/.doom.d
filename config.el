;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;https://meet.google.com/bqe-gucr-hqr; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; ======================== My Settings ========================
;;;; -*- Load settings Dirs -*-
(add-load-path! "settings")
(add-load-path! "settings/packages")

;;;; -*- Load settings files -*-
(use-package! my-mappings)

;; (setq doom-font (font-spec :family "Hasklig" :size 20))
(setq doom-font (font-spec :family "Iosevka SS04" :weight 'light :size 21))
(setq
 doom-big-font (font-spec :family "Iosevka SS04" :weight 'light :size 20)
 doom-variable-pitch-font (font-spec :family "Iosevka SS04" :weight 'light :size 18))
;; (setq doom-font (font-spec :family "JetBrains Mono" :size 19))
;; (setq  doom-big-font (font-spec :family "JetBrains Mono" :size 20))
;; (setq doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 18))

(setq doom-themes-enable-italics t)
(setq-default line-spacing 3)

;; Disable auto-fill-mode (hard wrapping) in text and mainly writeroom modes
(add-hook! 'text-mode-hook #'turn-off-auto-fill)
(add-hook! 'writeroom-mode-hook #'turn-off-auto-fill)
(add-hook! 'org-mode-hook #'turn-off-auto-fill)

;; js2 settings
(add-hook 'js2-mode-hook (lambda ()
    (setq +pretty-code-symbols-alist '(js2-mode nil ))))
(add-hook!
  js2-mode 'prettier-js-mode
  (add-hook 'before-save-hook #'refmt-before-save nil t))

;; these two settings center screen vertically around the cursor
(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 999)

;; Soft wrap
(global-visual-line-mode 1)
;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; Soft wrap at fill-column
;; (setq-default fill-column 80)
;; (global-visual-fill-column-mode)
;; Change left/right margin
;; (setq-default left-margin-width 10 right-margin-width 5)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Persist Emacs’ initial frame position, dimensions and/or full-screen state
;; across sessions
(when-let (dims (doom-store-get 'last-frame-size))
  (cl-destructuring-bind ((left . top) width height fullscreen) dims
    (setq initial-frame-alist
          (append initial-frame-alist
                  `((left . ,left)
                    (top . ,top)
                    (width . ,width)
                    (height . ,height)
                    (fullscreen . ,fullscreen))))))

(defun save-frame-dimensions ()
  (doom-store-put 'last-frame-size
                  (list (frame-position)
                        (frame-width)
                        (frame-height)
                        (frame-parameter nil 'fullscreen))))

(add-hook 'kill-emacs-hook #'save-frame-dimensions)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Enable highlight matching tag in web-mode
(use-package! web-mode
  :config
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;; Tabs
(use-package! centaur-tabs
  :config
  (setq centaur-tabs-style "rounded"
        ;; centaur-tabs-set-icons t
        ;; centaur-tabs-set-modified-marker nil
        ;; centaur-tabs-modified-marker "●"
        centaur-tabs-height 32
        centaur-tabs-set-bar 'over)
  ;; (centaur-tabs-headline-match)
  (centaur-tabs-mode)
  ;; (defun centaur-tabs-buffer-groups ()
  ;;   (list "GROUP"))
  (defun centaur-tabs-hide-tab (x)
    "Disable tabs for certain buffer types"
    (let ((name (format "%s" x)))
      (or
       (window-dedicated-p (selected-window))
       (string-prefix-p "*epc" name)
       (string-prefix-p "*Messages" name)
       (string-prefix-p "*scratch" name)
       (string-prefix-p "*org-roam" name)
       (string-prefix-p "*doom" name)
       (string-prefix-p "*which-key*" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*tide-server" name)
       (string-prefix-p "*lsp" name)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))
  :hook
  (org-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
)

;; Neotree
(use-package! neotree
  :config
  (setq neo-window-width 27)
  (map! :n "C-n" nil
        :n "C-n" #'neotree-toggle)
  (map! :n "C-f" nil
        :n "C-f" #'neotree-find)
  (map! :map neotree-mode-map
        :n [tab] (neotree-make-executor
                  :dir-fn  #'neo-open-dir
                  :file-fn #'neotree-quick-look)
        :n "DEL" #'evil-window-prev
        :n "n"   #'neotree-next-line
        :n "p"   #'neotree-previous-line
        :m "h"   #'+neotree/collapse-or-up
        :m "l"   #'+neotree/expand-or-open
        :n "J"   #'neotree-select-next-sibling-node
        :n "K"   #'neotree-select-previous-sibling-node
        :n "H"   #'neotree-select-up-node
        :n "L"   #'neotree-select-down-node
        :n "G"   #'evil-goto-line
        :n "gg"  #'evil-goto-first-line
        :n "v"   (neotree-make-executor :file-fn 'neo-open-file-vertical-split)
        :n "s"   (neotree-make-executor :file-fn 'neo-open-file-horizontal-split))
  )

;; evil-goggles (highlight after yank)
(use-package! evil-goggles
  :init
  (setq evil-goggles-duration 0.3)
  :config
  (custom-set-faces
   '(evil-goggles-yank-face
     ((t (:background "#ffd078" :foreground "#454545"))))))

;; evil-snipe
(use-package! evil-snipe
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer))

;; ======================== Writeroom ========================

;; Writeroom settings
(global-writeroom-mode 1)
(use-package! writeroom-mode
  :demand t
  :config
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
  (setq writeroom-width 80
        writeroom-mode-line t
        writeroom-header-line t
        visual-line-mode t
        writeroom-fullscreen-effect nil
        writeroom-maximize-window nil)
  (add-hook 'prog-mode-hook #'writeroom-mode)
)

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "logo.png"))

;; ======================== Orgmode ========================

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/pCloud/org/")
;; (setq org-agenda-files (list org-directory "~/pCloud/org/<name>"))
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(setq org-archive-location (concat org-directory ".archive-%s::"))
(setq org-agenda-skip-scheduled-if-done t)

(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package org-fancy-priorities
  ;; :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (setq org-fancy-priorities-list '("⚡" "⬆" "☕" "⬇" "❗"))
)

(after! org
 (setq
  ;; org-superstar-headline-bullets-list '("⁖")
  org-todo-keywords
  '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
  org-todo-keyword-faces
  '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
    ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
    ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
    ("CANCELLED" :foreground "#ff6480" :weight normal :underline t)
    ("DONE" :foreground "#50a14f" :weight normal :underline t))
  org-priority-faces '((65 :foreground "#e45649")
                       (66 :foreground "#da8548")
                       (67 :foreground "#0098dd"))
  org-ellipsis " ... "
  ;; org-ellipsis " ▾ "
 ))

(setq org-super-agenda-groups '((:name "Today"
                                 :time-grid t
                                 :scheduled today)
                                (:name "Due today"
                                 :deadline today)
                                (:name "Important"
                                 :priority "A")
                                (:name "Overdue"
                                 :deadline past)
                                (:name "Due soon"
                                 :deadline future)
                                (:name "Big Outcomes"
                                 :tag "bo")))

;; ======================== Org Roam ========================

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-jump-to-index
                             org-roam-graph)
  :init
  (setq org-roam-directory "~/pCloud/org/roam")
  (setq org-roam-index-file "~/pCloud/org/roam/20200707065858-index.org")
  (map! :leader :desc "org-roam-find-file" "rf" #'org-roam-find-file)
  (map! :leader :desc "org-roam-jump-to-index" "rj" #'org-roam-jump-to-index)
  (map! :leader
        :desc "org-roam-toggle-buffer" "/" #'org-roam-buffer-toggle-display
        :prefix ("r" . "org-roam")
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-graph
        :desc "org-roam-capture" "c" #'org-roam-capture)
  :config
  (setq org-roam-buffer-width 0.2)
  (setq org-roam-graph-viewer "/usr/bin/open")
)

;; (after! org-roam
;;   (push 'company-org-roam company-backends)
;;   (set-company-backend! 'org-mode 'company-capf 'company-org-roam)
;;   (set-company-backend! 'markdown-mode 'company-capf 'company-org-roam))

(use-package! company
  :config
  (setq company-idle-delay 0.1))

(use-package! deft
  :after org
  :init
  (map! :leader :prefix "r"
        :desc "Open Deft" "d" #'deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/pCloud/org/roam/"))

;; ======================== Themes ========================

(setq custom-safe-themes t)

;; (setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-nord-light)
;; (setq doom-theme 'zaiste)
;; (setq doom-theme 'leuven)
;; (load-theme 'less-black)
(load-theme 'less)
;; (load-theme 'less-jb)
;; (load-theme 'eink)
;; (use-package! base16)

;; ======================== END Own Settings ========================

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
(put 'customize-apropos 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#161719" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#ECEFF4"])
 '(custom-safe-themes
   (quote
    ("e1ca19a047c94342562bd55588b87d7ac1162ef5465a84af9cac1321724ce420" "f1938227a38cfe55a12076dac514f03a1d9aa1a47957870b690cc80db5e7a1b3" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" default)))
 '(fci-rule-color "#4C566A")
 '(jdee-db-active-breakpoint-face-colors (cons "#191C25" "#81A1C1"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#191C25" "#A3BE8C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#191C25" "#434C5E"))
 '(objed-cursor-color "#BF616A")
 '(package-selected-packages (quote (org-fancy-priorities base16-theme)))
 '(pdf-view-midnight-colors (cons "#ECEFF4" "#2E3440"))
 '(rustic-ansi-faces
   ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#88C0D0" "#ECEFF4"])
 '(vc-annotate-background "#2E3440")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A3BE8C")
    (cons 40 "#bbc28b")
    (cons 60 "#d3c68b")
    (cons 80 "#EBCB8B")
    (cons 100 "#e2b482")
    (cons 120 "#d99d79")
    (cons 140 "#D08770")
    (cons 160 "#c68984")
    (cons 180 "#bd8b98")
    (cons 200 "#B48EAD")
    (cons 220 "#b77f96")
    (cons 240 "#bb7080")
    (cons 260 "#BF616A")
    (cons 280 "#a05b67")
    (cons 300 "#815664")
    (cons 320 "#625161")
    (cons 340 "#4C566A")
    (cons 360 "#4C566A")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
