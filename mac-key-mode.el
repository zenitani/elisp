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
;; additional mac-like key bindings and extra elisp functions.
;;
;; To use this package, add these lines to your .emacs file:
;;
;;     (require 'mac-key-mode)
;;     (mac-key-mode 1)
;;


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

;; (defcustom mac-key-advanced-setting t
;;   "If non-nil, `mac-key-mode' activates addional settings:
;; 1) menu items are added to the File menu and the Edit menu, and
;; 2) the SPC key invokes Quick Look information in dired-mode."
;;   :group 'mac-key-mode
;;   :type 'boolean)

;; (defvar mac-key-backup-command-modifier nil
;;   "Internal variable.  Do not use this.")


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

        ;; (setq mac-key-backup-command-modifier mac-command-modifier)
        ;; (setq mac-command-modifier 'alt)
        ;; (if (boundp 'mac-key-mode-internal)
        ;;     (setq mac-key-mode-internal t))

        ;; ;; turn on advanced settings
        ;; (when mac-key-advanced-setting

          ;; menu items
          (define-key-after menu-bar-file-menu [mac-key-file-separator]
            '("--" . nil) 'recover-session)
          (define-key-after menu-bar-file-menu [mac-key-show-in-finder]
            '(menu-item "Show In Finder" mac-key-show-in-finder
              :help "Display current file/directory in a Finder window"
              :enable (or (and (boundp 'buffer-file-name) buffer-file-name)
                          (and (boundp 'dired-directory) dired-directory)))
            'mac-key-file-separator)
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

      ;; (setq mac-command-modifier mac-key-backup-command-modifier)
      ;; (if (boundp 'mac-key-mode-internal)
      ;;     (setq mac-key-mode-internal nil))

      ;; ;; turn off advanced settings
      ;; (when mac-key-advanced-setting

        ;; menu items
        (global-unset-key [menu-bar file mac-key-file-separator])
        (global-unset-key [menu-bar file mac-key-show-in-finder])
        (global-unset-key [menu-bar file mac-key-open-terminal])

        ;; restore SPC to dired-next-line (a bad way to deal with it)
        (if (boundp 'dired-mode-map)
            (define-key dired-mode-map " " 'dired-next-line))
        (remove-hook 'dired-mode-hook
                     (lambda () (interactive)
                       (define-key dired-mode-map " " 'mac-key-quick-look)))

        );)
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
            (if (fboundp 'mac-process-activate)
                (mac-process-activate "com.apple.finder")
              (do-applescript "tell application \"Finder\" to activate"))
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
            (if (fboundp 'mac-process-activate)
                (mac-process-activate "com.apple.Terminal")
              (do-applescript "tell application \"Terminal\" to activate"))
            )
        (error err))
      )
     (t (error "An error occured"))
     )))


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
