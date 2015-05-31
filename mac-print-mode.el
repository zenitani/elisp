;;; mac-print-mode.el --- Alt print dialog for Carbon/Cocoa Emacs

;; Copyright (C) 2005-2011  Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id$
;; based on htmlize-view by Lennart Borgman
;; http://ourcomments.org/Emacs/DL/elisp/htmlize-view.el

;; This file is distributed under the terms of the GNU General Public
;; License version 3, or (at your option) any later version.

;;; Commentary:

;; This package requires htmlize.el <http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el>.
;; To use,
;;
;; M-x mac-print-buffer
;; M-x mac-print-buffer-with-faces
;; or
;; (mac-print-mode 1)
;;

;;; Code:

(require 'htmlize)

(defvar mac-print-kill-view-buffers t
  "If non-nil, delete temporary html buffers after sending it to the TextEdit application.")

(defvar mac-print-font-size 8 "Font size, in points, for ordinary text, when printing by `mac-print-buffer' function. ")


;;;###autoload
(defun mac-print-buffer-with-faces(&optional region-only)
  "Convert buffer to html, preserving colors and decoration and
send it to the TextEdit application.
If REGION-ONLY is non-nil then only the region is printed."
  (interactive)
  (let* ((default-directory "~/") ;; When editing a remote file
         (htmlize-after-hook nil)
         (htmlize-generate-hyperlinks nil)
         (htmlize-output-type 'css)
         (htmlize-head-tags
          (concat "<style>\n" "pre { \n"
                  "font-size:" (int-to-string mac-print-font-size) "pt;\n"
                  "word-wrap:break-word;\n" ;; to wrap long lines
                  "}\n" "</style>\n"))
         )
    (message "printing...")
;;     (shell-command
;;      (concat (shell-quote-argument mac-print-coral-program)
;;              " -d " (mac-print-htmlize-buffer-to-tempfile region-only)))
    (do-applescript (concat "
tell application \"TextEdit.app\"
    activate
    try
        print alias (POSIX file \"" (mac-print-htmlize-buffer-to-tempfile region-only) "\") with print dialog
    end try
end tell
"))
    (message "printing... done")
    ))

;;;###autoload
(defun mac-print-buffer(&optional region-only)
  "Convert buffer to html, and then send it to the TextEdit application.
If REGION-ONLY is non-nil then only the region is printed."
  (interactive)
  (let* ((default-directory "~/") ;; When editing a remote file
         (htmlize-after-hook '(mac-print-monolize-html))
         (htmlize-generate-hyperlinks nil)
         (htmlize-output-type 'css)
         (htmlize-head-tags
          (concat "<style>\n" "pre { \n"
                  "font-size:" (int-to-string mac-print-font-size) "pt;\n"
                  "word-wrap:break-word;\n" ;; to wrap long lines
                  "}\n" "</style>\n"))
         )
    (message "printing...")
;;     (shell-command
;;      (concat (shell-quote-argument mac-print-coral-program)
;;              " -d " (mac-print-htmlize-buffer-to-tempfile region-only)))
    (do-applescript (concat "
tell application \"TextEdit.app\"
    activate
    try
        print alias (POSIX file \"" (mac-print-htmlize-buffer-to-tempfile region-only) "\") with print dialog
    end try
end tell
"))
    (message "printing... done")
    ))

(defun mac-print-monolize-html ()
  (narrow-to-region
   (search-forward "    <!--")
   (search-forward "-->"))
  (beginning-of-buffer)
  (replace-regexp "        color: \#.*" "")
  (replace-regexp "        background-color: \#.*" "")
  (widen)
  )

(defun mac-print-htmlize-buffer-to-tempfile(region-only)
  "Convert buffer to html, preserving colors and decoration.
If REGION-ONLY is non-nil then only the region is sent to the TextEdit application.
Return a cons with temporary file name followed by temporary buffer."
  (save-excursion
    (let (;; Just use Fundamental mode for the temp buffer
          magic-mode-alist
          auto-mode-alist
          (html-temp-buffer
           (if (not region-only)
               (htmlize-buffer (current-buffer))
             (let ((start (mark)) (end (point)))
               (or (<= start end)
                   (setq start (prog1 end (setq end start))))
               (htmlize-region start end))))
          (file (make-temp-file "emacs-print-" nil ".html")))
      (set-buffer html-temp-buffer)
      (write-file file nil)
      (if mac-print-kill-view-buffers (kill-buffer html-temp-buffer))
      file)))


(defadvice ps-print-buffer-with-faces
  (around mac-print-ad())
  (mac-print-buffer-with-faces))
(defadvice ps-print-region-with-faces
  (around mac-print-ad())
  (mac-print-buffer-with-faces t))
(defadvice ps-print-buffer
  (around mac-print-ad())
  (mac-print-buffer))
(defadvice ps-print-region
  (around mac-print-ad())
  (mac-print-buffer t))
(defadvice print-buffer (around mac-print-ad2 (&optional arg))
  (interactive "P")
  (mac-print-buffer-with-faces))
(defadvice print-region (around mac-print-ad2 (&optional arg))
  (interactive "P")
  (mac-print-buffer-with-faces t))

;;;###autoload
(define-minor-mode mac-print-mode
  "Toggle Mac Print mode."
  :global t
  (if mac-print-mode
      (ad-enable-regexp "mac-print-ad*")
    (ad-disable-regexp "mac-print-ad*")
    ))


(provide 'mac-print-mode)

;; mac-print-mode.el ends here.
