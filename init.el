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

;; Settings local to this computer
(load-file "~/.emacs.d/local.el")