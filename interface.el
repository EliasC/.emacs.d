(setq default-major-mode 'text-mode)
(setq-default fill-column 66)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq sentence-end-double-space nil)

;; Global auto revert mode
(global-auto-revert-mode t)

;; Show paren-mode
(show-paren-mode)

;; No toolbar, no scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Color
(load-theme 'wombat)
(set-background-color "black")

;; Font size
(set-face-attribute 'default nil :height 180)
(global-set-key [(super +)] 'text-scale-increase )
(global-set-key [(super -)] 'text-scale-decrease )

;; Window size
(global-set-key [(control super up)] 'enlarge-window)
(global-set-key [(control super down)] 'shrink-window)
(global-set-key [(control super right)] 'enlarge-window-horizontally)
(global-set-key [(control super left)] 'shrink-window-horizontally)

;; Global whitespace-mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; smex
(global-set-key "\M-x" 'smex)

; Shell Modeline Dirtrack
(defun add-mode-line-dirtrack ()
      (add-to-list
       'mode-line-buffer-identification
       '(:propertize (" " default-directory " ") face dired-directory)))

(add-hook 'shell-mode-hook 'add-mode-line-dirtrack)

; Uniquify buffernames
(require 'uniquify)

;; Workgroups
(require 'workgroups)
(workgroups-mode 1)
(wg-load "~/.emacs.d/wg")
(add-hook 'kill-emacs-hook 'wg-update-all-workgroups-and-save)

;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)