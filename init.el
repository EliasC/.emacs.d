;; Find this file

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun ec/init-file ()
  "Open ~/.emacs.d/init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Set up load and environment paths
(load-file "~/.emacs.d/paths.el")

;; Make Emacs look pretty
(load-file "~/.emacs.d/interface.el")

;; Make Emacs understand me
(load-file "~/.emacs.d/input.el")

;; Make Emacs guide me
(load-file "~/.emacs.d/nav.el")

;; Make Emacs understand programming
(load-file "~/.emacs.d/lang.el")

;; Make Emacs do useful/silly things
(load-file "~/.emacs.d/misc.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-face-groups nil)
 '(agda2-highlight-level (quote non-interactive))
 '(agda2-include-dirs (quote ("." "/Users/Elias/.cabal/bin")))
 '(agda2-program-name "/Users/Elias/.cabal/bin/agda")
 '(custom-safe-themes
   (quote
    ("bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" default)))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(org-src-fontify-natively t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "SteelBlue1"))))
 '(agda2-highlight-function-face ((t (:foreground "SteelBlue1"))))
 '(agda2-highlight-postulate-face ((t (:foreground "SteelBlue1"))))
 '(agda2-highlight-primitive-face ((t (:foreground "SteelBlue1"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "SteelBlue1"))))
 '(agda2-highlight-record-face ((t (:foreground "SteelBlue1"))))
 '(font-lock-warning-face ((t (:foreground "red2" :weight bold)))))
