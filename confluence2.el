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
(require 'ediff)

(defvar confluence-url nil)
(defvar confluence-default-space nil)

(defvar confluence-login-token nil)
(defvar confluence-space-history nil)
(defvar confluence-page-history nil)

(defvar confluence-before-save-hook nil)

(defvar confluence-page-struct nil)
(make-variable-buffer-local 'confluence-page-struct)
(put 'confluence-page-struct 'permanent-local t)

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

(defun confluence-get-parent-page ()
  (interactive)
  (let ((parent-page-id (cf-get-struct-value confluence-page-struct "parentId" "0")))
    (if (equal parent-page-id "0")
        (message "Current page has no parent page")
      (cf-show-page (confluence-execute 'confluence1.getPage parent-page-id)))))

(defun confluence-create-page-by-path (page-path)
  (interactive "MPageSpace/PageName: ")
  (confluence-create-page (cf-get-space-name page-path)
                          (cf-get-page-name page-path)))

(defun confluence-create-page (&optional space-name page-name)
  (interactive)
  (let ((new-page (list (cons "space" 
                                                (or space-name
                                                    (cf-prompt-space-name)))
                                          (cons "title"
                                                (or page-name
                                                    (cf-prompt-page-name)))
                                          (cons "content" "")))
        (parent-page-id (cf-get-parent-page-id)))
    (if parent-page-id
        (setq new-page (append new-page (list (cons "parentId" parent-page-id)))))
    (cf-show-page (confluence-execute 'confluence1.storePage new-page))))

(defun confluence-ediff-merge-current-page ()
  (interactive)
  (cf-ediff-current-page t))

(defun confluence-ediff-current-page ()
  (interactive)
  (cf-ediff-current-page nil))

(defun confluence-reparent-page ()
  (interactive)
  (let ((parent-page-id (cf-get-parent-page-id)))
    (if (and parent-page-id
             (not (equal parent-page-id (cf-get-struct-value confluence-page-struct "parentId"))))
        (progn
          (cf-set-struct-value confluence-page-struct "parentId" parent-page-id)
          (set-buffer-modified-p t)))))

(defun confluence-rename-page ()
  (interactive)
  (let ((page-name (cf-prompt-page-name "New ")))
    (if (and (> (length page-name) 0)
             (not (equal page-name (cf-get-struct-value confluence-page-struct "title"))))
        (progn
          (cf-set-struct-value confluence-page-struct "title" page-name)
          (rename-buffer (format "%s<%s>"
                                 (cf-get-struct-value confluence-page-struct "title")
                                 (cf-get-struct-value confluence-page-struct "space")))
          (set-buffer-modified-p t)
          ))))

(defun cf-ediff-current-page (update-cur-version)
  (if (null confluence-page-id)
      (error "Could not diff Confluence page %s, missing page id"
             (buffer-name)))
  (let ((rev-buf)
        (cur-buf (current-buffer))
        (rev-page (confluence-execute 'confluence1.getPage confluence-page-id)))
    (setq rev-buf
          (get-buffer-create (format "%s.~%s~" (buffer-name cur-buf) (cf-get-struct-value rev-page "version" 0))))
    (with-current-buffer rev-buf
      (cf-insert-page rev-page)
      (toggle-read-only 1)
      )
    (if update-cur-version
        (setq confluence-page-struct (cf-set-struct-value (copy-alist rev-page) "content" "")))
    (ediff-buffers cur-buf rev-buf nil 'confluence-diff)))

(defun cf-save-page ()
  (if (null confluence-page-id)
      (error "Could not save Confluence page %s, missing page id"
             (buffer-name)))
  (widen)
  (run-hooks 'confluence-before-save-hook)
  (cf-insert-page (confluence-execute 'confluence1.storePage
                                      (cf-set-struct-value (copy-alist confluence-page-struct) "content"
                                                           (buffer-string)))
                  t)
  t)

(defun cf-revert-page (&optional arg noconfirm)
  (if (and confluence-page-id
           (or noconfirm
               (yes-or-no-p "Revert Confluence Page ")))
      (cf-insert-page (confluence-execute 'confluence1.getPage
                                          confluence-page-id))))

(defun cf-show-page (full-page)
  (let* ((page-buf-name (format "%s<%s>"
                               (cf-get-struct-value full-page "title")
                               (cf-get-struct-value full-page "space")))
         (page-buffer (get-buffer page-buf-name)))
    (if (not page-buffer)
        (progn
          (setq page-buffer (get-buffer-create page-buf-name))
          (set-buffer page-buffer)
          (cf-insert-page full-page)
          (goto-char (point-min))))
    (switch-to-buffer page-buffer)))

(defun cf-insert-page (full-page &optional keep-undo)
  (let ((old-point (point)))
    (widen)
    (erase-buffer)
    (setq confluence-page-id (cf-get-struct-value full-page "id"))
    (insert (cf-get-struct-value full-page "content" ""))
    (setq confluence-page-struct (cf-set-struct-value full-page "content" ""))
    (set-buffer-modified-p nil)
    (or keep-undo
        (eq buffer-undo-list t)
        (setq buffer-undo-list nil))
    (confluence-mode)
    (goto-char old-point)))

(defun cf-prompt-space-name (&optional prompt-prefix)
  (let ((space-prompt (if confluence-default-space
                          (format "Page Space [%s]: " confluence-default-space)
                        "Page Space: ")))
    (read-string (concat (or prompt-prefix "") space-prompt) nil 'confluence-space-history
                 confluence-default-space t)))

(defun cf-prompt-page-name (&optional prompt-prefix)
  (read-string (concat (or prompt-prefix "") "Page Name: ") nil 'confluence-page-history nil t))

(defun cf-get-parent-page-id ()
  (if (and confluence-page-id
           (yes-or-no-p "Use current page for parent "))
      confluence-page-id
    (let ((parent-space-name (or (cf-get-struct-value confluence-page-struct "space")
                                 (cf-prompt-space-name "Parent ")))
          (parent-page-name (cf-prompt-page-name "Parent ")))
      (if (and (> (length parent-space-name) 0)
               (> (length parent-page-name) 0))
          (cf-get-struct-value (confluence-execute 'confluence1.getPage parent-space-name parent-page-name) "id")
        nil))))

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

(defun cf-get-struct-value (struct key &optional default-value)
  (or (and struct
           (cdr (assoc key struct)))
      default-value))

(defun cf-set-struct-value (struct key value)
  (setcdr (assoc key struct) value)
  struct)

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
           (string-match "[&\r]" string))
      (save-excursion
	(set-buffer (get-buffer-create " *Confluence-decode*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(insert string)
	(goto-char (point-min))
        (while (re-search-forward "&\\([^;]+\\);" nil t)
          (let ((ent-str (match-string 1)))
            (replace-match (cond
                            ((cdr-safe (assoc ent-str
                                              '(("quot" . ?\")
                                                ("amp" . ?&)
                                                ("lt" . ?<)
                                                ("gt" . ?>)))))
                            ((save-match-data
                               (and (string-match "^#\\([0-9]+\\)$" ent-str)
                                    (string (string-to-number (match-string 1 ent-str))))))
                            ((save-match-data
                               (and (string-match "^#x\\([0-9A-Fa-f]+\\)$" ent-str)
                                    (string (string-to-number (match-string 1 ent-str) 16)))))
                            (t ent-str))
                           t t)))

	(goto-char (point-min))
        (while (re-search-forward "\r\n" nil t)
          (replace-match "\n" t t))
	(buffer-string))
    string))


(defconst confluence-keywords
  (list
  
   '("{\\([^}]+\\)}"
     (1 font-lock-constant-face prepend))
  
   '("{warning}\\(.*?\\){warning}"
     (1 font-lock-warning-face prepend))
  
   ;; bold
   '("[ ][*]\\([^*]+\\)[*][ ]"
     (1 'bold prepend t))
   
   ;; code
   '("{{\\([^*]+\\)}}"
     (1 doxygen-code-face prepend t))
   
   ;; italics/emphasised
   '("[ ]_\\([^_]+\\)_[ ]"
     (1 'italic prepend t))

   ;; underline
   '("[ ][+]\\([^+]+\\)[+][ ]"
     (1 'underline prepend t))

   ;; headings
   '("^h1[.] \\(.*\\)$"
     (1 '(bold underline) prepend t))
   '("^h2[.] \\(.*\\)$"
     (1 '(bold italic underline) prepend t))
   '("^h3[.] \\(.*\\)$"
     (1 '(italic underline) prepend t))
   '("^h[4-9][.] \\(.*\\)$"
     (1 'underline prepend t))


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
  (make-local-variable 'make-backup-files)
  (setq make-backup-files nil)
  (setq font-lock-defaults
        '(confluence-keywords nil nil nil nil))
  (add-hook 'write-contents-hooks 'cf-save-page)
  (setq buffer-file-name (expand-file-name (buffer-name) "~/"))
)

(provide 'confluence2)
;;; confluence2.el ends here
