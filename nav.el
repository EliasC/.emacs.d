;; Back to indentation with M-a
(global-set-key "\M-a" 'back-to-indentation)

;; Cycle frames
(defun other-other-window ()
  (interactive)
  (other-window -1)
)

(global-set-key [(super up)] 'other-other-window)
(global-set-key [(super down)] 'other-window)

;; Mark stack
;; (setq-default set-mark-command-repeat-pop t)
;; C-SPC after C-u C-SPC cycles mark stack

;; Remember my position
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers
(require 'saveplace)                          ;; get the package

;; Navigate mode for home row arrow-keys. Elias (2013-05-15)
(define-minor-mode navigate-mode
  "When Navigate mode is enabled, the keys H,T,N,C can be used
   like the arrow-keys. D and S move to the beginning/end of the
   line, and G and R move up/down whole paragraphs"
  :lighter " Nav"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "c") 'previous-line)
            (define-key map (kbd "t") 'next-line)
            (define-key map (kbd "h") 'backward-word)
            (define-key map (kbd "n") 'forward-word)
            (define-key map (kbd "g") 'backward-paragraph)
            (define-key map (kbd "r") 'forward-paragraph)
            (define-key map (kbd "d") 'beginning-of-line)
            (define-key map (kbd "s") 'end-of-line)
            (define-key map (kbd "l") 'recenter-top-bottom)
            map)
)
;; QWERTY-version
;; (define-minor-mode navigate-mode
;;   "When Navigate mode is enabled, the keys J,K,L,I can be used
;;    like the arrow-keys. H and Ö move to the beginning/end of the
;;    line, and U and O move up/down whole paragraphs"
;;   :lighter " Nav"
;;   :keymap (let ((map (make-sparse-keymap)))
;;          (define-key map (kbd "i") 'previous-line)
;;          (define-key map (kbd "k") 'next-line)
;;          (define-key map (kbd "j") 'backward-word)
;;          (define-key map (kbd "l") 'forward-word)
;;          (define-key map (kbd "u") 'backward-paragraph)
;;          (define-key map (kbd "o") 'forward-paragraph)
;;          (define-key map (kbd "h") 'beginning-of-line)
;;          (define-key map (kbd "ö") 'end-of-line)
;;             map)
;; )
(global-set-key "\C-x\C-n" 'navigate-mode)
(key-chord-define-global "hn" 'navigate-mode)

;; iy-go-to-char (Ace jump vinner... eller...?)

(global-set-key (kbd "H-s") 'iy-go-up-to-char)
(global-set-key (kbd "H-r") 'iy-go-up-to-char-backward)

;; Ace jump mode
;(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20130720.1153/")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(define-key global-map (kbd "C-j") 'ace-jump-mode)
(define-key global-map (kbd "H-l") 'ace-jump-line-mode)
(define-key global-map (kbd "H-j") 'ace-jump-char-mode)

;; Switch window
(global-set-key (kbd "H-o") 'switch-window)

;; imenu
(setq imenu-auto-rescan t)
(global-set-key (kbd "C-.") 'imenu-anywhere)
