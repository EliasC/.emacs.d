;; gpg
(setq epg-gpg-program "/usr/local/bin/gpg")

;; Mail
(setq
send-mail-function 'smtpmail-send-it
message-send-mail-function 'smtpmail-send-it
user-mail-address "elias.castegren@it.uu.se"
smtpmail-starttls-credentials '(("smtp.uu.se" 25 nil nil))
smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
smtpmail-default-smtp-server "smtp.uu.se"
smtpmail-smtp-server "smtp.uu.se"
smtpmail-smtp-service 587
smtpmail-debug-info t
starttls-extra-arguments nil
starttls-gnutls-program "/usr/local/bin/gnutls-cli"
smtpmail-warn-about-unknown-extensions t
starttls-use-gnutls t)

;; Make emacs mail work like it is used
(defun compose-mail-current-buffer ()
 "Turn the current buffer into an email"
 (interactive)
 (if (region-active-p)
     (kill-ring-save (region-beginning) (region-end))
   (kill-ring-save (buffer-end -1) (buffer-end +1)))
 (compose-mail)
 (goto-line 5)
 (yank)
 (goto-line 1)
 (move-end-of-line 1))

(global-set-key "\C-xm" 'compose-mail-current-buffer)

;; Create nice PDFs from code on screen. Elias and Tobias (2013-05-13)
(defun print-to-pdf (&optional use-bg-p)
  "Print current buffer or region to PDF with faces"
  (interactive "P")
  (let ((tmp-ps        (make-temp-file "ptp" nil ".ps"))
        (tmp-uncropped (make-temp-file "ptp" nil "-before-crop.pdf"))
        (tmp-pdf       (make-temp-file "ptp" nil ".pdf"))
        ;; Cache the existing settings for Ps Print
        (old-ps-default-bg     (if (boundp 'ps-default-bg)     ps-default-bg     (nil)))
        (old-ps-landscape-mode (if (boundp 'ps-landscape-mode) ps-landscape-mode (nil)))
        (old-ps-print-header   (if (boundp 'ps-print-header)   ps-print-header   (nil)))
        (old-ps-left-margin    (if (boundp 'ps-left-margin)    ps-left-margin    (nil)))
        (old-ps-font-size      (if (boundp 'ps-font-size)      ps-font-size      (nil))))
    ;; Change the global defaults for the duration of this command
    (setq ps-landscape-mode t)
    (setq ps-font-size 8.0)
    (setq ps-print-header nil)
    (if use-bg-p (progn
                   (setq ps-left-margin 0)
                   (setq ps-default-bg "#3f3f3f")))
    ;; Actually perform the printing
    (if (region-active-p)
        (ps-print-with-faces (region-beginning) (region-end) tmp-ps nil)
      (ps-print-with-faces (buffer-end -1) (buffer-end +1) tmp-ps nil))
    (shell-command (format "sed -i.bak s/[cC]ourier/Inconsolata/g %s" tmp-ps))
    (shell-command (format "sed -i.bak s/\"LineSpacing[ ]*0\"/\"LineSpacing 2\"/g %s" tmp-ps))
    (shell-command (format "ps2pdf14 -sPAPERSIZE=a4 %s %s" tmp-ps (if (region-active-p) tmp-uncropped tmp-pdf)))
    (if (region-active-p)
        (shell-command (format "pdfcrop -margins \"3 3 3 3\" %s %s" tmp-uncropped tmp-pdf)))
    (shell-command (format "open %s" tmp-pdf))
    ;; Revert global defaults to their old values
    (if old-ps-landscape-mode (setq ps-landscape-mode old-ps-landscape-mode))
    (if old-ps-font-size      (setq ps-font-size      old-ps-font-size))
    (if old-ps-print-header   (setq ps-print-header   old-ps-print-header))
    (if old-ps-left-margin    (setq ps-left-margin    old-ps-left-margin))
    (if old-ps-default-bg     (setq ps-default-bg     old-ps-default-bg))
    (message "Created a PDF file")))

;; Emacspeak
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
   is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun ec/speak ()
  "Make Emacs say (and kill) what you have marked. Works on whole
   lines by default"
  (interactive)
  (progn
    (if (not (region-active-p))
        (select-current-line)
      ())
      (let ((s (replace-regexp-in-string
                "\'" "\\\\'" (buffer-substring-no-properties
                              (region-end) (region-beginning))))
            (this (current-buffer))
            (buffer-modified-p nil))
        (kill-region (region-end) (region-beginning))
        (switch-to-buffer "*Async Shell Command*")
        (async-shell-command (concat "say " s))
        (switch-to-buffer this))))

(global-set-key (kbd "H-å") 'ec/speak)
