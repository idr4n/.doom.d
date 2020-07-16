;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync' on the command line, then restart Emacs for the changes to take effect -- or use 'M-x doom/reload'.
;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; Themes
(package! base16-theme)
(package! eink-theme)
;; (package! less-theme
;;   :recipe (:host gitlab :repo "nobiot/less-theme"
;;            :files ("*.el" "less")))

;; Writeroom
(package! writeroom-mode)

;; prettier
(package! prettier-js)

;; Org Mode related
(package! org-fancy-priorities)
(package! org-super-agenda)
(package! company-org-roam
  :recipe (:host github :repo "org-roam/company-org-roam"))
(package! org-roam-server)
(unpin! org-roam)
(unpin! company-org-roam)

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

