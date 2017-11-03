;; ======
;; elisp
;; ======

;; eval-region/eval-defun using the same binding
(global-set-key (kbd "H-e") (lambda () (interactive)
                              (if (region-active-p)
                                  (progn
                                   (eval-region (region-beginning) (region-end))
                                   (deactivate-mark))
                                (eval-defun nil))))

;; Eval and replace
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x C-e") 'eval-and-replace)


;; ======
;; LaTeX
;; ======

;; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t) ;; pdflatex

(setq TeX-view-program-list
   '(("Preview" "open -a Preview %o")
     ("Skim"    "/Applications/Skim.app/Contents/SharedSupport/displayline -r %n %o %b")))
(setq TeX-view-program-selection '((output-pdf "Skim")))

;; LaTeX customisation - Tobias
(defun tw/latex-customization ()
 "Hook for latex stuff"
 (interactive)
 ;; Make auctex work as I like it to
 (local-set-key (kbd "C-c C-e") 'LaTeX-environment)
 (local-set-key (kbd "C-c e") 'LaTeX-close-environment)
 ;; Jumps to the right place in Skim.app using C-c C-v (2010-11-19)
 (local-set-key (kbd "<C-return>") 'tw/auctex-view)
 ;; Mac-Specific
 (add-to-list 'TeX-expand-list '("%u" tw/skim-box-in-quotes))
 (add-to-list 'TeX-view-program-list '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %u"))
 (add-to-list 'TeX-view-program-selection '(output-pdf "Skim"))
 ;; Compile currently open file with AucTeX
 (global-set-key (kbd "M-<return>") 'tw/auctex-compile))

;; View with working revert
(defun tw/skim-force-revert ()
 "Tell Skim to revert -- useful when file watch was lost due to new object replacing old"
 (interactive)
 (shell-command (format "osascript \
 -e 'tell application \"Skim\"' \
 -e 'revert front document' \
 -e 'tell front document to go to TeX line %s from POSIX file \"%s\"' \
 -e 'end tell';"
                         (TeX-current-line)
                         (buffer-file-name))))

;; AucTeX Compilation
(defun tw/auctex-compile ()
 "Compile master file"
 (interactive)
 (TeX-command "LaTeX" 'TeX-master-file))

;; Show current tex master in Skim
(defun tw/auctex-view (&optional revert-p)
 "Show the current tex master in Skim, prefix command forces revert"
 (interactive "P")
 (if revert-p
     (tw/skim-force-revert)
   (TeX-view)))

;; Creates the argument vector to the viwer, inserts quotes to
;; handle spaces in file names
(defun tw/skim-box-in-quotes ()
 (concat
  (TeX-current-line)
  " \""
  (expand-file-name (funcall file (TeX-output-extension) t)
                    (file-name-directory (TeX-master-file)))
  "\" \""
  (buffer-file-name)
  "\""
  ))

(provide 'latex-customisation)

;; ======
;; Modes
;; ======

;; prog-mode

;; prog-mode hook for C-c C-c compile
(dolist (hook '(prog-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-c")
                             'compile))))

;; prog-mode hook for next-error
(dolist (hook '(prog-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-n")
                             'next-error))))

;; prog-mode hook for previous-error
(dolist (hook '(prog-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-p")
                             'previous-error))))


;; Coq mode
(setq auto-mode-alist (cons '("\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)


;; Proof General
(load-file "/usr/local/share/emacs/site-lisp/proof-general/generic/proof-site.el")
(dolist (hook '(coq-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-h")
                             'proof-goto-point))))

;; Scala mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/scala-mode")

(require 'scala-mode-auto)

;; K mode
;(setq load-path (cons "/Users/Elias/k/share/editor-support/emacs" load-path))
;(load-library "k3-mode")
;(add-to-list 'auto-mode-alist '("\\.k$" . k3-mode)) ;; to launch k3-mode for .k files

;; Ott mode
(require 'ottmode)
(require 'ott-extras)

;; Agda mode
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

(setq ec/normal-meta-p t)
(defun ec/agda-meta ()
  ""
  (interactive)
  (setq mac-command-modifier  'meta)
  (setq mac-option-modifier  'nil))

(defun ec/normal-meta ()
  ""
  (interactive)
  (setq mac-command-modifier  'super)
  (setq mac-option-modifier  'meta))

(defun ec/swap-meta ()
  ""
  (interactive)
  (if ec/normal-meta-p
      (progn (ec/agda-meta) (setq ec/normal-meta-p nil))
    (progn (ec/normal-meta) (setq ec/normal-meta-p t))))

(global-set-key (kbd "C-H-S") 'ec/swap-meta)

;; Flychecker
(require 'flycheck)

;; Flymake
;; Display flymake errors in minibuffer
(custom-set-variables
     '(help-at-pt-timer-delay 0.9)
     '(help-at-pt-display-when-idle '(flymake-overlay)))

;; prog-mode hook for C-c C-r flymake-next-error
(dolist (hook '(prog-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-r")
                             'flymake-goto-next-error))))

;; prog-mode hook for C-c C-g flymake-prev-error
(dolist (hook '(prog-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-g")
                             'flymake-goto-prev-error))))

;; Flycheck (Can't get hlint to work)
; (add-hook 'after-init-hook #'global-flycheck-mode)
; (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
; (eval-after-load 'flycheck '(require 'flycheck-ghcmod))

;; Haskell-mode
(dolist (hook '(haskell-mode-hook))
      (add-hook hook (lambda () (haskell-indent-mode 1))))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook
   (function
    (lambda ()
      (setq haskell-program-name "ghci")
      (setq haskell-ghci-program-name "ghci6"))))

(add-hook 'haskell-mode-hook
   (function
    (lambda () (flymake-hlint-load))))

(defun ec/try-haskell-process-cabal-build ()
    "Try to build, start a session if it fails"
  (interactive)
  (if (haskell-session-maybe)
      (haskell-process-cabal-build)
    (progn
      (haskell-session-change)
      (haskell-process-cabal-build))))

(add-hook 'haskell-mode-hook
   (function
    (lambda ()
      (local-set-key (kbd "C-c C-c") 'ec/try-haskell-process-cabal-build))))


;; flymake-hlint-replace
(require 'pulse)

(defvar ec/flymake-hlint-re "Found:\n *\\(.*\\)\nWhy not:\n *\\(.*\\)")

(defun ec/flymake-hlint-replace ()
  "Parse the current hlint message provided by flymake and try to
   perform the suggested change automatically"
  (interactive)
  (let*
      ((value (assoc (line-number-at-pos) flymake-err-info))
       (_ (cond ((null value) (error "No error or warning for current line"))))
       (record (caadr value))
       (msg (elt record 4))
       (re (string-match ec/flymake-hlint-re msg))
       (_ (cond ((null re) (error "I can't fix this automatically, sorry =/"))))
       (current (match-string 1 msg))
       (current-safe (replace-regexp-in-string " +" " *" (regexp-quote current)))
       (new (match-string 2 msg)))
    (progn
      (beginning-of-line)
      (if (re-search-forward-lax-whitespace current-safe nil t)
          (replace-match new)
        (error "*** Bug: Could not find text to replace! ***"))
      (pulse-momentary-highlight-region (match-beginning 0) (point))
      (sit-for 2) ; block help-at-pt-timer to let flymake notice the change
      )))

(dolist (hook '(haskell-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c <C-return>")
                             'ec/flymake-hlint-replace))))

;; Ignore Haskell-interface files
(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".dyn_hi")
(add-to-list 'completion-ignored-extensions ".dyn_o")

;; Java hook for C-c C-c compile
(dolist (hook '(java-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-c")
                             'compile))))

;; Rust hook for C-c C-c compile
(dolist (hook '(rust-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-c")
                             'compile))))

;; C hook for next-error
(dolist (hook '(c-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-n")
                             'next-error))))
;; orc-mode
(require 'orc-mode)

;; Cool mode (for the Coursera compilers course)
(require 'cool-mode)

;; Encore mode
(add-to-list 'load-path "~/Dropbox/PhD/encore/emacs/encore-mode/")
(require 'encore-mode)
(setq encore-block-highlight-toggle t)

;; Flyspell mode
(setq-default ispell-program-name "/usr/local/bin/ispell")
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(latex-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(java-mode-hook))
      (add-hook hook (lambda () (flyspell-prog-mode))))
(dolist (hook '(c-mode-hook))
      (add-hook hook (lambda () (flyspell-prog-mode))))

(dolist (hook '(c-mode-hook))
  (add-hook hook
            (lambda () (interactive)
              (local-set-key (kbd "C-c C-c")
                             'compile))))
