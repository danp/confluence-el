;;; confluence2.el --- Emacs mode for interacting with confluence wikie

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: James Ahlborn <jahlborn@boomi.com>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; 

;;; Code:

(require 'xml-rpc)

(defvar confluence-url nil)
(defvar confluence-space nill)
(defvar confluence-login-token nil)
(defvar confluence-page-struct nil)
(make-variable-buffer-local 'confluence-page-struct)

(defun confluence-execute (method-name &rest params)
  (confluence-login)
  (apply 'confluence-execute-internal method-name confluence-login-token params))

(defun confluence-execute-internal (method-name &rest params)
  (let ((url-http-version "1.0")
        (url-http-attempt-keepalives nil))
    (url-decode-entities-in-value (apply 'xml-rpc-method-call confluence-url method-name params))))

(defun confluence-login ()
  (interactive)
  (if (not confluence-login-token)
      (setq confluence-login-token
            (confluence-execute-internal 'confluence1.login
                                         (read-string "Username: " user-login-name)
                                         (read-passwd "Password: ")))
        ))

(defun confluence-get-page (page-name)
  (interactive "MPageName: ")
  (let ((full-page (confluence-execute 'confluence1.getPage confluence-space page-name))
        (page-buffer))
    (setq page-buffer (get-buffer-create (format "<%s>%s"
                                                 (cf-get-struct-value full-page "space")
                                                 (cf-get-struct-value full-page "title"))))
    (set-buffer page-buffer)
    (delete-region (point-min) (point-max))
    (insert (cf-get-struct-value full-page "content"))
    (cf-set-struct-value full-page "content" nil)
    (setq confluence-page-struct full-page)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (switch-to-buffer page-buffer)
    ))



(defun cf-get-struct-value (struct key)
  (cdr (assoc key struct)))

(defun cf-set-struct-value (struct key value)
  (setcdr (assoc key struct) value))

(defun url-decode-entities-in-value (value)
  (cond ((listp value)
         (dolist (struct-val value)
           (setcdr struct-val (url-decode-entities-in-value (cdr struct-val)))))
        ((stringp value)
         (setq value (url-decode-entities-in-string value))))
  value)

(defun url-decode-entities-in-string (string)
  "Convert HTML entities with string values:
    &amp;  ==>  &
    &lt;   ==>  <
    &gt;   ==>  >
    &quot; ==>  \"
    &#32   ==>  ' '"
  (if (and (stringp string)
           (string-match "[&]" string))
      (save-excursion
	(set-buffer (get-buffer-create " *entity*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(insert string)
	(goto-char (point-min))
        (while (re-search-forward "&[^;]+;" nil t)
          (replace-match (cdr-safe (assoc (match-string 0)
                                '(("&quot;" . ?\")
			       ("&amp;" . ?&)
			       ("&lt;" . ?<)
			       ("&gt;" . ?>)
                               ;; FIXME, this should handle any hex code
                               ("&#32;" . " "))))
                         t t))
	(buffer-string))
    string))


(provide 'confluence2)
;;; confluence2.el ends here
