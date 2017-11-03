;; fn --> hyper
(setq mac-function-modifier 'hyper)

;; Space instead of tabs
(setq-default indent-tabs-mode nil)

;; No electric indent (why do I suddenly need this?)
(electric-indent-mode 0)

;; ido mode
(ido-mode)

(setq ido-auto-merge-delay-time 9999)

;; Wrap electric
(defun ec/wrap-electric (left right)
  ""
  (interactive)
  (if (region-active-p)
      (let ((start (region-beginning)) (end (region-end)))
        (progn
          (goto-char end) (insert right)
          (goto-char start) (insert left)))
    (progn
      (insert left)
      (save-excursion
        (insert right))
      )))

;; Special characters
(setq ec/delimiters '(("(" ")")
                      ("[" "]")
                      ("{" "}")
                      ("\"" "\"")
                      ("$" "$")
                      ("|" "|")
                      ("<" ">")))

(defun ec/has-delim (delimiters delim)
  (and (not (null delimiters))
       (or (equal delim (car (car delimiters)))
           (ec/has-delim (cdr delimiters) delim))))

(defun ec/matching-delim (delimiters delim)
  "Extract the right delimeter matching delim"
  (if (equal delim (car (car delimiters)))
      (cadr (car delimiters))
    (ec/matching-delim (cdr delimiters) delim)))

(defun ec/delimiters (delim)
  "Insert delim or wrap in delims"
  (interactive)
  (if (and (region-active-p) (ec/has-delim ec/delimiters delim))
      (ec/wrap-electric delim (ec/matching-delim ec/delimiters delim))
    (insert delim)))

;; Teach Emacs Swedish keyboard layout
(define-key key-translation-map (kbd "M-8") (kbd "["))
(define-key key-translation-map (kbd "M-9") (kbd "]"))

(define-key key-translation-map (kbd "M-(") (kbd "{"))
(define-key key-translation-map (kbd "M-)") (kbd "}"))

(define-key key-translation-map (kbd "M-7") (kbd "|"))
(define-key key-translation-map (kbd "M-/") (kbd "\\"))

(define-key key-translation-map (kbd "M-2") (kbd "@"))
(define-key key-translation-map (kbd "M-4") (kbd "$"))

(global-set-key "(" (lambda () (interactive) (ec/delimiters "(")))
(global-set-key "[" (lambda () (interactive) (ec/delimiters "[")))
(global-set-key "<" (lambda () (interactive) (ec/delimiters "<")))
(global-set-key "{" (lambda () (interactive) (ec/delimiters "{")))

(global-set-key "|" (lambda () (interactive) (ec/delimiters "|")))

(global-set-key "\"" (lambda () (interactive) (ec/delimiters "\"")))

(global-set-key "$" (lambda () (interactive) (ec/delimiters "$")))

(global-set-key (kbd "H-8")
                (lambda () (interactive) (ec/wrap-electric "(" ")")))
(global-set-key (kbd "H-M-8")
                (lambda () (interactive) (ec/wrap-electric "[" "]")))
(global-set-key (kbd "H-M-(")
                (lambda () (interactive) (ec/wrap-electric "{" "}")))
(global-set-key (kbd "H-9")
                (lambda () (interactive) (ec/wrap-electric "{" "}")))
(global-set-key (kbd "H-2")
                (lambda () (interactive) (ec/wrap-electric "\"" "\"")))
(global-set-key (kbd "H-4")
                (lambda () (interactive) (ec/wrap-electric "$" "$")))
(global-set-key (kbd "H-<")
                (lambda () (interactive) (ec/wrap-electric "<" ">")))
(global-set-key (kbd "H-'")
                (lambda () (interactive) (ec/wrap-electric "'" "'")))
(global-set-key (kbd "H-*")
                (lambda () (interactive) (ec/wrap-electric "*" "*")))

;; Tab-expand
(global-set-key [(control tab)] 'hippie-expand)
(setq hippie-expand-try-functions-list
  '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-dabbrev-from-kill try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; Remove Line
(global-set-key [(super backspace)] "\C-a\C-\s\C-e\d")

;; Move entire paragraph with M-n/M-p
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)

;; Key Chords
;(add-to-list 'load-path "~/.emacs.d/elpa/key-chord-20080915.2156/")
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.05)
(setq key-chord-one-key-delay 0.125)

;; Visual Regexp
(global-set-key "\M-%" 'vr/query-replace)

;; YASnippets
;(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20140106.1009/")
(require 'yasnippet) ;; not yasnippet-bundle

(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/snippets")
(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                         "~/Dropbox/PhD/encore/emacs/encore-mode/snippets"))
(yas-reload-all)

(setq-default mode-require-final-newline nil)
(setq yas-triggers-in-field 'true)

; Hook to let yas-mode play nicely with org-mode
(defun yas/org-very-safe-expand ()
            (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
                    (lambda ()
                      (make-variable-buffer-local 'yas/trigger-key)
                      (setq yas/trigger-key [tab])
                      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                      (define-key yas/keymap [tab] 'yas/next-field)))

;; Replace region by blanks
(defun replace-region-by-blanks (start end)
  "Replace each character in the region with a space"
  (interactive "r")
  (if (region-active-p)
         (progn (copy-region-as-kill start end)
                (replace-regexp "." " " nil start end)
                (message ""))
    (message "Mark is not active")
  ))

(global-set-key (kbd "H-w") 'replace-region-by-blanks)

;; Chrome: Edit with Emacs
;(add-to-list 'load-path "~/.emacs.d/elpa/edit-server-20130927.1643/")
(require 'edit-server)
(edit-server-start)
(when (and (require 'edit-server nil t) (daemonp))
  (edit-server-start))

;; Multiple cursors
;(add-to-list 'load-path "~/.emacs.d/elpa/multiple-cursors-20130913.1148/")
    (require 'multiple-cursors)

(global-set-key (kbd "H-n") 'mc/mark-next-word-like-this)
(global-set-key (kbd "H-p") 'mc/mark-previous-word-like-this)

(global-set-key (kbd "H-M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "H-M-p") 'mc/mark-previous-like-this)

(global-set-key (kbd "H-s-n") 'mc/unmark-next-like-this)
(global-set-key (kbd "H-s-p") 'mc/unmark-previous-like-this)

(global-set-key (kbd "H-h") 'mc/mark-all-words-like-this)
(global-set-key (kbd "H-M-h") 'mc/mark-all-like-this)

(global-set-key (kbd "<H-mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "H-SPC") 'mc/edit-lines)

;; Expand region
;(add-to-list 'load-path "~/.emacs.d/elpa/expand-region-20130911.319/")
(require 'expand-region)
(global-set-key (kbd "M-h") 'er/expand-region)

;; Trim region
(defun ec/trim-region (left right)
  "If region is active, delete the left- and rightmost character
in it. If not, use expand-region once and delete the selection"
  (interactive "r")
  (if (region-active-p)
      (let (deactivate-mark)
        (goto-char right)
        (delete-backward-char 1)
        (goto-char left)
        (delete-char 1)
        )
    (progn (er/expand-region 1)
           (kill-region (region-end) (region-beginning)))))

(global-set-key (kbd "H-d") 'ec/trim-region)

;; Manual highlight
(setq ec/highlight-colors '("hi-blue" "hi-pink" "hi-green" "hi-yellow"))
(setq ec/highlight-color-index 0)

(defun ec/highlight-region ()
  "Highlight the current region with the next color in the highlight-colors list.
   Disable highlighting and reset color-index if region is not active"
  (interactive)
  (if (region-active-p)
      (progn
        ;; Select next available color and highlight selected text
        (highlight-phrase (buffer-substring-no-properties (mark) (point))
                          (nth ec/highlight-color-index ec/highlight-colors))
        (setq ec/highlight-color-index (mod (+ 1 ec/highlight-color-index)
                                            (length ec/highlight-colors)))
        (deactivate-mark))
    (progn
      ;; Disable highlighting
      (hi-lock-mode (- 1))
      (setq ec/highlight-color-index 0))))

(global-set-key (kbd "H-M-l") 'ec/highlight-region)

;; Auto complete mode
;(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20130724.1750/")
;(add-to-list 'load-path "~/.emacs.d/elpa/popup-20130708.2245/")
(require 'popup)
; (require 'auto-complete)
; (global-set-key (kbd "C-<return>")
;                 (lambda () (interactive)
;                   (progn (auto-complete-mode 1) (auto-complete))))
; (add-to-list 'ac-dictionary-directories
;              "~/.emacs.d/elpa/auto-complete-20130724.1750/dict")

;; Align
(global-set-key (kbd "H-a") 'align)
(global-set-key (kbd "H-M-a") 'align-regexp)

;; Support for marking a rectangle of text with highlighting.
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)
(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)

;; move-text
(global-set-key (kbd "H-c") 'move-text-up)
(global-set-key (kbd "H-t") 'move-text-down)

;; Magit-bindings
(define-prefix-command 'magit-map)
(global-set-key (kbd "M-m") 'magit-map)
(global-set-key (kbd "M-m M-s") 'magit-status)
(global-set-key (kbd "M-m M-l") 'magit-log)
(global-set-key (kbd "M-m M-p") 'magit-pull)
(global-set-key (kbd "M-m M-f") 'magit-fetch)
