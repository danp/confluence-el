;;; confluence2.el --- Emacs mode for interacting with confluence wikie

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: James Ahlborn <jahlborn@boomi.com>
;; Keywords: confluence, wiki, jscheme, xmlrpc

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
(defvar confluence-default-space nil)

(defvar confluence-login-token nil)
(defvar confluence-space-history nil)
(defvar confluence-page-history nil)

(defvar confluence-page-struct nil)
(make-variable-buffer-local 'confluence-page-struct)

(defvar confluence-page-id nil)
(make-variable-buffer-local 'confluence-page-id)
(put 'confluence-page-id 'permanent-local t)

(defun confluence-execute (method-name &rest params)
  (confluence-login)
  (apply 'confluence-execute-internal method-name confluence-login-token params))

(defun confluence-execute-internal (method-name &rest params)
  (let ((url-http-version "1.0")
        (url-http-attempt-keepalives nil))
    (url-decode-entities-in-value (apply 'xml-rpc-method-call confluence-url method-name params))))

(defun confluence-login (&optional arg)
  (interactive "P")
  (if (or (not confluence-login-token)
          arg)
      (setq confluence-login-token
            (confluence-execute-internal 
             'confluence1.login
             (read-string "Username: " user-login-name)
             (read-passwd "Password: ")))
        ))

(defun confluence-get-page-by-path (page-path)
  (interactive "MPageSpace/PageName: ")
  (confluence-get-page (cf-get-space-name page-path)
                       (cf-get-page-name page-path)))

(defun confluence-get-page (&optional space-name page-name)
  (interactive)
  (cf-show-page (confluence-execute 'confluence1.getPage
                                    (or space-name
                                        (cf-prompt-space-name))
                                    (or page-name
                                        (cf-prompt-page-name)))))

(defun confluence-create-page-by-path (page-path)
  (interactive "MPageSpace/PageName: ")
  (confluence-create-page (cf-get-space-name page-path)
                          (cf-get-page-name page-path)))

(defun confluence-create-page (&optional space-name page-name)
  (interactive)
  (cf-show-page (confluence-execute 'confluence1.storePage
                                    (list (cons "space" 
                                                (or space-name
                                                    (cf-prompt-space-name)))
                                          (cons "title"
                                                (or page-name
                                                    (cf-prompt-page-name)))
                                          (cons "content" "")))))

(defun cf-save-page ()
  (if (null confluence-page-id)
      (error "Could not save Confluence page %s, missing page id"
             (buffer-name)))
  (widen)
  (cf-set-struct-value confluence-page-struct "content"
                       (buffer-string))
  (cf-insert-page (confluence-execute 'confluence1.storePage
                                      confluence-page-struct)))

(defun cf-revert-page (&optional arg noconfirm)
  (if (and confluence-page-id
           (or noconfirm
               (yes-or-no-p "Revert Confluence Page ")))
      (let ((old-point (point))
            (new-page (confluence-execute 'confluence1.getPage
                                          confluence-page-id)))
        (cf-insert-page new-page)
        (goto-char old-point))))

(defun cf-show-page (full-page)
  (let ((page-buffer (get-buffer-create 
                       (format "%s<%s>"
                               (cf-get-struct-value full-page "title")
                               (cf-get-struct-value full-page "space")))))
    (set-buffer page-buffer)
    (cf-insert-page full-page)
    (goto-char (point-min))
    (switch-to-buffer page-buffer)))

(defun cf-insert-page (full-page)
  (widen)
  (kill-all-local-variables)
  (erase-buffer)
  (setq confluence-page-id (cf-get-struct-value full-page "id"))
  (insert (cf-get-struct-value full-page "content"))
  (cf-set-struct-value full-page "content" nil)
  (setq confluence-page-struct full-page)
  (set-buffer-modified-p nil)
  (or (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (confluence-mode))

(defun cf-prompt-space-name ()
  (let ((space-prompt (if confluence-default-space
                          (format "Page Space [%s]: " confluence-default-space)
                        "Page Space: ")))
    (read-string space-prompt nil 'confluence-space-history
                 confluence-default-space t)))

(defun cf-prompt-page-name ()
  (read-string "Page Name: " nil 'confluence-page-history nil t))

(defun cf-get-space-name (page-path)
  (let ((page-paths (split-string page-path "/")))
    (if (> (length page-paths) 1)
        (car page-paths)
      confluence-default-space)))

(defun cf-get-page-name (page-path)
  (let ((page-paths (split-string page-path "/")))
    (if (> (length page-paths) 1)
        (cadr page-paths)
      page-path)))

(defun cf-get-struct-value (struct key)
  (cdr (assoc key struct)))

(defun cf-set-struct-value (struct key value)
  (setcdr (assoc key struct) value))

(defun url-decode-entities-in-value (value)
  (cond ((listp value)
         (dolist (struct-val value)
           (setcdr struct-val 
                   (url-decode-entities-in-value (cdr struct-val)))))
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


(defconst confluence-keywords
  (list
  
   '("{\\([^}]+\\)}"
     (1 font-lock-constant-face prepend))
  
   '("{warning}\\(.*?\\){warning}"
     (1 font-lock-warning-face prepend))
  
   ;; bold
   '("[*]\\([^*]+\\)[*]"
     (1 (quote bold) prepend t))
   
   ;; code
   '("{{\\([^*]+\\)}}"
     (1 doxygen-code-face prepend t))
   
   ;; italics/emphasised
   '("_\\([^_]+\\)_"
     (1 (quote italic) prepend t))

   ;; underline
   '("[+]\\([^+]+\\)[+]"
     (1 (quote underline) prepend t))

   ;; headings
   '("^h[1-9][.] \\(.*\\)$"
     (1 (quote bold) prepend t))

  ;; links
   '("\\[\\(\\([^|]*\\)[|]\\)?\\([^]]+\\)\\]"
     (2 font-lock-string-face prepend t t)
     (3 (quote underline) prepend t)
     )
   )
  
  )

(define-derived-mode confluence-mode text-mode "Confluence"
  "Set major mode for editing Confluence Wiki pages."
  (turn-off-auto-fill)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'cf-revert-page)
  (setq font-lock-defaults
        '(confluence-keywords nil nil nil nil))
  (add-hook 'write-contents-hooks 'cf-save-page)
)

(provide 'confluence2)
;;; confluence2.el ends here
