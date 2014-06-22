;;; -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006, 2012  by Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id: utf-tool.el 756 2012-06-13 14:11:21Z zenitani $
;; URL: http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#utf-sty

;; utf パッケージ (utf.sty) を便利に使うための関数を提供します。
;; M-x utf-sty-encode-buffer, utf-sty-encode-region は
;; バッファ・リージョン内のテキストを \UTF{...} などに変換します。
;; M-x utf-sty-decode-buffer, utf-sty-decode-region は
;; その逆の処理を行います。
;; 
;; 例）森鷗外 ⇔ 森\UTF{9DD7}外
;; 
;; また、prefix（C-u）を付けて使うこともできます。
;; C-u M-x utf-sty-encode-buffer = M-x utf-sty-decode-buffer

;; このファイルは GPL ライセンス v2 のもとで再配布可能です。

;;; Code:

(defun utf-sty-encode-buffer (&optional arg)
  (interactive "*p")
  (utf-sty-encode-region (point-min) (point-max) arg)
  )
(defun utf-sty-decode-buffer (&optional arg)
  (interactive "*p")
  (utf-sty-decode-region (point-min) (point-max) arg)
  )

(defun utf-sty-encode-region (start end &optional arg)
  (interactive "*r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (cond
       ((= arg 1) (utf-sty-encode1)) ; M-x
       ((= arg 4) (utf-sty-decode1)) ; C-u M-x
       ))))

(defun utf-sty-decode-region (start end &optional arg)
  (interactive "*r\np")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (cond
       ((= arg 1) (utf-sty-decode1)) ; M-x
       ((= arg 4) (utf-sty-encode1)) ; C-u M-x
       ))))

(defun utf-sty-encode1()
  (goto-char (point-min))
  (while (not (eobp))
    (let* ((char (char-after))
           (charset (char-charset char))
           ;; (charset-description charset)
           ;; (split (split-char char))
           (pos (point))
           (unicode nil))
      (unless
          (memq charset '(ascii japanese-jisx0208 katakana-jisx0201))
        (if (or (< char 256)
                (memq 'mule-utf-8 (find-coding-systems-region pos (1+ pos)))
                (get-char-property pos 'untranslated-utf-8))
            (setq unicode (or (get-char-property pos 'untranslated-utf-8)
                              (encode-char char 'ucs))))
        (when unicode
          (delete-char 1)
          (cond
           ((eq charset 'korean-ksc5601)
            (insert-string (format "\\UTFK{%04X}" unicode)))
           ((eq charset 'chinese-gb2312)
            (insert-string (format "\\UTFC{%04X}" unicode)))
           ((eq charset 'chinese-big5-1)
            (insert-string (format "\\UTFT{%04X}" unicode)))
           (t
            (insert-string (format "\\UTF{%04X}" unicode)))
           ))
        )
      (if (not (eobp))(forward-char))
      )))

(defun utf-sty-decode1()
  (goto-char (point-min))
  (while (re-search-forward
          "\\\\\\(UTF\\|UTFK\\|UTFT\\|UTFC\\)\{\\([0-9a-f][0-9a-f][0-9a-f][0-9a-f]\\)\}"
          nil t 1)
    (let ((str (match-string 2)))
      (replace-match ""  t nil)
      (if (stringp str) (ucs-insert str))
      )))

(provide 'utf-tool)

;; utf-tool.el ends here.