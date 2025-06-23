;;; mac-key-mode.el --- mac-style key bindings for Emacs  -*- lexical-binding: nil -*-

;; Copyright (C) 2004-2010, 2025  Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@gmail.com>
;; Keywords: tools, mac
;; Created: 2004-12-27
;; Compatibility: Emacs 30 on macOS
;; URL: https://github.com/zenitani/elisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides mac-key-mode, a minor mode that provides
;; additional mac-like key bindings and elisp functions.
;;
;; To use this package, add these lines to your .emacs file:
;;
;;     (require 'mac-key-mode)
;;     (mac-key-mode 1)
;;
;; With help from htmlize.el <https://github.com/hniksic/emacs-htmlize>,
;; it provides the following printing functions.
;;
;; * M-x mac-key-print-buffer
;; * M-x mac-key-print-buffer-with-faces


;;; Code:

(defgroup mac-key-mode nil
  "Mac-style key-binding mode."
  :group 'ns)
(defconst mac-key-mode-lighter
  (char-to-string 8984) ;; the command mark
  ;; (char-to-string 127822) ;; the Apple mark (Emoji)
  "A lighter string which is displayed in the modeline
when `mac-key-mode' is on.")

(defcustom mac-key-mode-hook nil
  "The hook to run when mac-key-mode is toggled."
  :type 'hook
  :group 'mac-key-mode)


(defcustom mac-key-printing t
  "If non-nil, activate printing functions.
This requires htmlize.el <https://github.com/hniksic/emacs-htmlize>."
  :group 'mac-key-mode
  :type 'boolean)

(defcustom mac-key-print-kill-view-buffers t
  "If non-nil, delete the temporary buffer after sending it to TextEdit.app,
when printing with the `mac-key-print-buffer' functions. "
  :group 'mac-key-mode
  :type 'boolean)

(defcustom mac-key-print-font-size 8
  "Font size, in points, for ordinary text, for `mac-key-printing'. "
  :group 'mac-key-mode)


;; process objects
(defvar mac-key-speech-process nil
  "The process object for text-to-speech subprocess.")
(defvar mac-key-ql-process nil
  "The process object for Quick Look subprocess.")


(defvar mac-key-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\s-w] 'mac-key-close-window)
    (define-key map [?\s-Z] 'undo-redo)
    (define-key map [?\s-i] 'mac-key-show-in-finder)
    (define-key map [?\s-p] 'mac-key-print-buffer-with-faces)
    (define-key map [?\s-/] 'info)
    (define-key map [?\s-.] 'keyboard-quit)
    (define-key map [s-up] 'beginning-of-buffer)
    (define-key map [s-down] 'end-of-buffer)
    (define-key map [s-mouse-1] 'browse-url-at-mouse)
    (define-key map [C-down-mouse-1] 'mac-key-context-menu)
    ;; (define-key map [mouse-3] 'mac-key-context-menu)
    ;; (define-key map [A-S-mouse-1] 'mouse-buffer-menu)
    ;; (define-key map [S-down-mouse-1] 'mac-key-shift-mouse-select)
    map)
  "Keymap for `mac-key-mode'.")

;; mode-line menu
(define-key-after mode-line-mode-menu [mac-key-mode]
  `(menu-item ,(purecopy
                (concat "Mac Key (" mac-key-mode-lighter ")"))
              mac-key-mode :button (:toggle . mac-key-mode))
  'highlight-changes-mode)

;;;###autoload
(define-minor-mode mac-key-mode
  "Toggle Mac Key mode.
With arg, turn Mac Key mode on if arg is positive.
When Mac Key mode is enabled, mac-style key bindings are provided."
  :global t
  :group 'mac-key-mode
  :lighter (" " mac-key-mode-lighter)
  :keymap mac-key-mode-map
  (if mac-key-mode
      (progn

        ;; menu items
        (define-key-after menu-bar-file-menu [mac-key-file-separator1]
          '("--" . nil) 'recover-session)
        (define-key-after menu-bar-file-menu [mac-key-print-buffer-color]
          '(menu-item "Print Buffer" mac-key-print-buffer-with-faces
		      :help "Print current file/directory via TextEdit.app"
		      :enable (and mac-key-printing (featurep 'htmlize)))
          'mac-key-file-separator1)
        (define-key-after menu-bar-file-menu [mac-key-print-buffer-mono]
          '(menu-item "Print Buffer (Mono)" mac-key-print-buffer
		      :help "Print current file/directory via TextEdit.app"
		      :enable (and mac-key-printing (featurep 'htmlize)))
          'mac-key-print-buffer-color)
        (define-key-after menu-bar-file-menu [mac-key-file-separator2]
          '("--" . nil) 'mac-key-print-buffer-mono)
        (define-key-after menu-bar-file-menu [mac-key-show-in-finder]
          '(menu-item "Show In Finder" mac-key-show-in-finder
		      :help "Display current file/directory in a Finder window"
		      :enable (or (and (boundp 'buffer-file-name) buffer-file-name)
				  (and (boundp 'dired-directory) dired-directory)))
          'mac-key-file-separator2)
        (define-key-after menu-bar-file-menu [mac-key-open-terminal]
          '(menu-item "Open Terminal" mac-key-open-terminal
		      :help "Launch Terminal.app and go to the relevant directory")
          'mac-key-show-in-finder)

        ;; assign mac-key-quick-look to the SPC key
        (if (boundp 'dired-mode-map)
            (define-key dired-mode-map " " 'mac-key-quick-look)
          (add-hook 'dired-mode-hook
                    (lambda () (interactive)
                      (define-key dired-mode-map " " 'mac-key-quick-look)))
          )
	  
        );)
    (progn

      ;; menu items
      (global-unset-key [menu-bar file mac-key-file-separator1])
      (global-unset-key [menu-bar file mac-key-print-buffer-color])
      (global-unset-key [menu-bar file mac-key-print-buffer-mono])
      (global-unset-key [menu-bar file mac-key-file-separator2])
      (global-unset-key [menu-bar file mac-key-show-in-finder])
      (global-unset-key [menu-bar file mac-key-open-terminal])

      ;; restore SPC to dired-next-line (a bad way to deal with it)
      (if (boundp 'dired-mode-map)
          (define-key dired-mode-map " " 'dired-next-line))
      (remove-hook 'dired-mode-hook
                   (lambda () (interactive)
                     (define-key dired-mode-map " " 'mac-key-quick-look)))

      )
    ))


;; close window (command + W)
(defun mac-key-close-window ()
  "Close the Quick Look window or kill the current buffer."
  (interactive)
  (let ((mybuffer (and mac-key-ql-process
                       (process-buffer mac-key-ql-process))))
    (if (buffer-live-p mybuffer)
        (kill-buffer mybuffer))
    (kill-this-buffer)
    ))


;; Show In Finder (command + I)
(defun mac-key-show-in-finder (&optional path)
  "Display current file/directory in a Finder window"
  (interactive)
  (let ((item (or path
                  (and (boundp 'buffer-file-name) buffer-file-name)
                  (and (eq major-mode 'dired-mode) default-directory)) ))
    (cond
     ((not (stringp item)))
     ((file-remote-p item)
      (error "This item is located on a remote system."))
     (t
      (setq item (expand-file-name item))
      (condition-case err
          (progn
            (do-applescript
             (concat
              "tell application \"Finder\" to select (\""
              item "\" as POSIX file)"))
            (do-applescript "tell application \"Finder\" to activate")
            )
        (error err)))

     )))


;; Open Terminal.app
(defun mac-key-open-terminal (&optional path)
  "Launch Terminal and go to the relevant directory"
  (interactive)
  (let ((item (or path default-directory)))

    (cond
     ((not (stringp item)))
     ((file-remote-p item)
      (error "This item is located on a remote system."))
     ((file-directory-p item)
      (setq item (expand-file-name item))
      (condition-case err
          (progn
            (do-applescript
             (concat "tell application \"Terminal\" to do script"
                     " with command \"cd \" & quoted form of \""
		     item "\"" ))
            (do-applescript "tell application \"Terminal\" to activate")
            )
        (error err))
      )
     (t (error "An error occured"))
     )))


;; Print buffer contents (command + P)
(defun mac-key-print-buffer-with-faces(&optional region-only)
  "Convert buffer contents to html, preserving colors and decoration, and
print it via TextEdit.app.
If REGION-ONLY is non-nil then only the region is printed."
  (interactive)
  (if (not (featurep 'htmlize))
      (message "It doesn't work, because htmlize is not available.")

    (let* ((default-directory "~/") ;; When editing a remote file
           (htmlize-after-hook nil)
           (htmlize-generate-hyperlinks nil)
           (htmlize-output-type 'css)
           (htmlize-head-tags
            (concat "<style>\n" "pre { \n"
                    "font-size:" (int-to-string mac-key-print-font-size) "pt;\n"
                    "word-wrap:break-word;\n" ;; to wrap long lines
                    "}\n" "</style>\n"))
           )
      (message "printing...")
      (do-applescript "tell application \"TextEdit.app\" to activate")
      (do-applescript (concat "
tell application \"TextEdit.app\"
    try
        print alias (POSIX file \"" (mac-key-print-htmlize-buffer-to-tempfile region-only) "\") with print dialog
    end try
end tell
"))
      (message "printing... done")
    )))

;; Print buffer contents (no color)
(defun mac-key-print-buffer(&optional region-only)
  "Convert buffer contents to html, and then print it via TextEdit.app.
If REGION-ONLY is non-nil then only the region is printed."
  (interactive)
  (if (not (featurep 'htmlize))
      (message "It doesn't work, because htmlize is not available.")

    (let* ((default-directory "~/") ;; When editing a remote file
           (htmlize-after-hook '(mac-key-print-monolize-html))
           (htmlize-generate-hyperlinks nil)
           (htmlize-output-type 'css)
           (htmlize-head-tags
            (concat "<style>\n" "pre { \n"
                    "font-size:" (int-to-string mac-key-print-font-size) "pt;\n"
                    "word-wrap:break-word;\n" ;; to wrap long lines
                    "}\n" "</style>\n"))
           )
      (message "printing...")
      (do-applescript "tell application \"TextEdit.app\" to activate")
      (do-applescript (concat "
tell application \"TextEdit.app\"
    try
        print alias (POSIX file \"" (mac-key-print-htmlize-buffer-to-tempfile region-only) "\") with print dialog
    end try
end tell
"))
      (message "printing... done")
      )))

(defun mac-key-print-monolize-html ()
  (narrow-to-region
   (search-forward "    <!--")
   (search-forward "-->"))
  (goto-char (point-min))
  (replace-regexp "        color: \#.*" "")
  (replace-regexp "        background-color: \#.*" "")
  (widen)
  )

(defun mac-key-print-htmlize-buffer-to-tempfile(region-only)
  "Convert buffer contents to html, preserving colors and decoration.
If REGION-ONLY is non-nil then only region contents are htmlized.
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
      (if mac-key-print-kill-view-buffers (kill-buffer html-temp-buffer))
      file)))



;; Text-to-Speech functions
(defun mac-key-speak-buffer ()
  "Speak buffer contents."
  (interactive)
  (mac-key-speak-region (point-min)(point-max)))

(defun mac-key-speak-region (beg end)
  "Speak the region contents."
  (interactive "r")
  (mac-key-stop-speaking)
  (let ((buffer-file-coding-system 'utf-8-unix)
        (tmp-file (make-temp-file "emacs-speech-" nil ".txt")))
    (write-region beg end tmp-file nil)
    (message "Invoking text-to-speech...")
    (setq mac-key-speech-process
          (start-process "text-to-speech" "*Text-to-Speech Output*"
                         "/usr/bin/say" "-f" tmp-file))
    ))

(defun mac-key-stop-speaking ()
  "Terminate the text-to-speech subprocess, if it is running."
  (interactive)
  (let ((mybuffer (and mac-key-speech-process
                       (process-buffer mac-key-speech-process))))
    (when (buffer-live-p mybuffer)
      (kill-buffer mybuffer)
      (beep))
    ))


;; Quick Look
;; inspired by https://news.mynavi.jp/article/osx-263/
(defun mac-key-quick-look ()
  "Display the Quick Look information for the current line\'s file.
You might use dired-mode-hook to use this function in dired mode,
like this:

    (add-hook \'dired-mode-hook
       (lambda() (local-set-key \" \" \'mac-key-quick-look)))
"
  (interactive)
  
  (let ((mybuffer (and mac-key-ql-process
                       (process-buffer mac-key-ql-process)))
        (item default-directory))
    (cond
;;       (eq (process-status mac-key-ql-process) 'run)
;;       (kill-process mac-key-ql-process))
     ((file-remote-p item)
      (error "This item is located on a remote system."))
     (t
      (if (buffer-live-p mybuffer)
	  (kill-buffer mybuffer))
      (setq item (expand-file-name item))
      (condition-case err
          (setq item (dired-get-file-for-visit))
        (error err))
      (condition-case err
          (setq mac-key-ql-process
                (start-process "quicklook" "*QuickLook Output*"
                               "/usr/bin/qlmanage" "-p"
                               (shell-quote-argument item)))
        (error err)))
     )))


;; Contextual menu
(defun mac-key-context-menu (event)
  "Pop up a contextual menu."
  (interactive "e")

  (let ((editable (not buffer-read-only))
        (pt (save-excursion (mouse-set-point last-nonmenu-event)))
        beg end
        )

    ;; getting word boundaries
    (if (and mark-active
             (<= (region-beginning) pt) (<= pt (region-end)) )
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char pt)
        (setq end (progn (forward-word) (point)))
        (setq beg (progn (backward-word) (point)))
        ))

    ;; popup menu
    (popup-menu
     '(nil
       ;; ["Search in Spotlight"
       ;;  (mac-spotlight-search (buffer-substring-no-properties beg end))
       ;;  :active (fboundp 'mac-spotlight-search)
       ;;  :help "Do a Spotlight search of word at cursor"]
       ["Search in Google"
        (browse-url
         (concat "http://www.google.com/search?q="
                 (url-hexify-string (buffer-substring-no-properties beg end))))
        :help "Ask a WWW browser to do a Google search"]
     ["--" nil]
     ["Look Up in Dictionary"
      (browse-url
       (concat "dict:///"
               (url-hexify-string (buffer-substring-no-properties beg end))))
      :active t
      :help "Look up word at cursor in Dictionary.app"]
     ["--" nil]
     ["Cut"   (clipboard-kill-region beg end) :active (and editable mark-active)
      :help "Delete text in region and copy it to the clipboard"]
     ["Copy"  (clipboard-kill-ring-save beg end) :active mark-active
      :help "Copy text in region to the clipboard"]
     ["Paste" (clipboard-yank) :active editable
      :help "Paste text from clipboard"]
     ["--" nil]
     ("Spelling"
      ["Spelling..."
       (progn (goto-char end)(ispell-word)) :active editable
       :help "Spell-check word at cursor"]
      ["Check Spelling" (ispell-buffer) :active editable
       :help "Check spelling of the current buffer"]
      ["Check Spelling as You Type"
       (flyspell-mode)
       :style toggle :selected flyspell-mode :active editable
       :help "Check spelling while you edit the text"]
     )
     ("Font"
      ["Show Fonts" (ignore) :active nil]
      ["Bold"       (ignore) :active nil]
      ["Italic"     (ignore) :active nil]
      ["Underline"  (ignore) :active nil]
      ["Outline"    (ignore) :active nil]
      ["Styles..."  (ignore) :active nil]
      ["--" nil]
      ["Show Colors" (ignore) :active nil]
     )
     ("Speech"
      ["Start Speaking"
       (if (and mark-active
                (<= (region-beginning) pt) (<= pt (region-end)) )
           (mac-key-speak-region beg end)
         (mac-key-speak-buffer) )
       :help "Speak text through the sound output"]
      ["Stop Speaking" (mac-key-stop-speaking)
       :active (and mac-key-speech-process
                    (eq (process-status mac-key-speech-process) 'run))
       :help "Stop speaking"]
     )
     ["--" nil]
     ["Buffers" mouse-buffer-menu
       :help "Pop up a menu of buffers for selection with the mouse"]
     ))))


(provide 'mac-key-mode)

;;; mac-key-mode.el ends here.
