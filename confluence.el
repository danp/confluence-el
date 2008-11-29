;;; confluence.el --- Emacs mode for interacting with confluence wikie

;; Copyright (C) 2008  Free Software Foundation, Inc.

;; Author: James Ahlborn
;; Author: Kyle Burton <kyle.burton@gmail.com>
;; Keywords: confluence, wiki, xmlrpc

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
;; DOWNLOADING
;;
;; This module is available at Google Code:
;;
;;   http://code.google.com/p/confluence-el/
;;
;; INSTALLATION 
;;
;; You must set confluence-url in your .emacs file before using the
;; functions in this module.
;;
;; Some examples:
;;
;;   ;; loading xml-rpc.el may not be necessary, it depends on your
;;   ;; installed version of Emacs, it was necessary on 22.1.1
;;
;;   (load (expand-file-name "~/software/emacs/confluence-el/xml-rpc.el"))
;;   (load (expand-file-name "~/software/emacs/confluence-el/confluence.el"))
;;   (setq confluence-url "http://intranet/confluence/rpc/xmlrpc")
;;
;; USING CONFLUENCE MODE
;;
;; To open a page, M-x confluence-get-page and enter the path to the
;; page, for example, to open a page in your home space: ~username/Tasks
;;
;; It is often convienient to bind this to a global key \C-xwf in your .emacs file:
;;   
;;    (global-set-key "\C-xwf" 'confluence-get-page)
;;
;; Once you have opened a page, made changes, simply saving the page
;; ("\C-x\C-s") will push the changes back to the wiki.
;;
;; To view the changes in your page versus what is in the wiki, type
;; \C-xw=, or run M-x confluence-ediff-current-page.
;;
;; Also, if you want keybindings for confluence-mode, you can put the
;; following in your .emacs file:
;;
;; (add-hook 'confluence-mode-hook
;;   (local-set-key "\C-xw" confluence-prefix-map)
;;   (local-set-key "\M-j" 'confluence-newline-and-indent)
;;   (local-set-key "\M-;" 'confluence-list-indent-dwim))
;;
;; LONGLINES
;;
;;   http://www.emacswiki.org/emacs-en/LongLines
;;
;; Confluence uses a wiki-markup that treats linebreask as <p> HTML
;; tags.  Since this is the case, it is very common for paragraphs in
;; the Confluence markup to wrap around your buffer multiple times.
;; The LongLines mode allows those lines to be viewed within Emacs
;; with 'soft' linebreaks - which are inserted automatically, or via
;; M-q.  This makes it much more pleasant to work with large
;; paragraphs of text in the Confluence markup without introducing
;; unwanted paragraphs.
;;
;; See below for more advice on using LongLines and confluence-mode.
;;
;;
;; EXAMPLE .emacs CONFIGURATION
;;
;; (load (expand-file-name "~/software/emacs/confluence-el/xml-rpc.el"))
;; (load (expand-file-name "~/software/emacs/confluence-el/confluence.el"))
;;
;; ;; note, all customization must be in *one* custom-set-variables block
;; (custom-set-variables
;;  ;; ... other custimization
;;
;;  ;; confluence customization
;;  '(confluence-url "http://intranet/confluence/rpc/xmlrpc")
;;  '(confluence-default-space-alist (list (cons confluence-url "your-default-space-name"))))
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; confluence editing support (with longlines mode)
;;
;; (autoload 'confluence-get-page "confluence" nil t)
;;
;; (eval-after-load "confluence"
;;   '(progn
;;      (require 'longlines)
;;      (progn
;;        (add-hook 'confluence-mode-hook 'longlines-mode)
;;        (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
;;        (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
;;        (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))
;;
;; ;; LongLines mode: http://www.emacswiki.org/emacs-en/LongLines
;; (autoload 'longlines-mode "longlines" "LongLines Mode." t)
;;
;; (eval-after-load "longlines"
;;   '(progn
;;      (defvar longlines-mode-was-active nil)
;;      (make-variable-buffer-local 'longlines-mode-was-active)
;;
;;      (defun longlines-suspend ()
;;        (if longlines-mode
;;            (progn
;;              (setq longlines-mode-was-active t)
;;              (longlines-mode 0))))
;;
;;      (defun longlines-restore ()
;;        (if longlines-mode-was-active
;;            (progn
;;              (setq longlines-mode-was-active nil)
;;              (longlines-mode 1))))
;;
;;      ;; longlines doesn't play well with ediff, so suspend it during diffs
;;      (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
;;                                              activate compile preactivate)
;;        "Suspend longlines when running ediff."
;;        (with-current-buffer (ad-get-arg 0)
;;          (longlines-suspend)))
;;
;;     
;;      (add-hook 'ediff-cleanup-hook 
;;                '(lambda ()
;;                   (dolist (tmp-buf (list ediff-buffer-A
;;                                          ediff-buffer-B
;;                                          ediff-buffer-C))
;;                     (if (buffer-live-p tmp-buf)
;;                         (with-current-buffer tmp-buf
;;                           (longlines-restore))))))))
;;
;; 

;;; Code:

(require 'xml-rpc)
(require 'ediff)
(require 'thingatpt)
(require 'browse-url)

(defmacro confluence-coding-system-base (coding-system)
  "Safely returns the base of the given CODING-SYSTEM (or the given value if not found)."
  `(if (coding-system-p ,coding-system)
       (coding-system-base ,coding-system)
     ,coding-system))

(defgroup confluence nil
  "Support for editing confluence wikis."
  :prefix "confluence-")

(defcustom confluence-url nil
  "Url of the confluence service to interact with.  This must
point to the XML-RPC api URL for your confluence installation.

If your confluence installation is at http://intranet/confluence,
then the XML-RPC URL is probably
http://intranet/confluence/rpc/xmlrpc.  Setting this in your
.emacs is necessary before interacting with the Wiki."
  :group 'confluence
  :type 'string)

(defcustom confluence-default-space-alist nil
  "AList of default confluence spaces to use ('url' -> 'space')."
  :group 'confluence
  :type '(alist :key-type string :value-type string))

(defcustom confluence-search-max-results 20
  "Maximum number of results to return from a search."
  :group 'confluence
  :type 'integer)

(defcustom confluence-prompt-page-function 'cf-prompt-page-by-component
  "The function to used to prompt for pages when opening new pages."
  :group 'confluence
  :type 'function)

(defcustom confluence-min-page-completion-length 3
  "The minimum number of characters at which to attempt page completion.  Set to -1 to disable page completion."
  :group 'confluence
  :type 'integer)

(defcustom confluence-min-page-repeat-completion-length 3
  "The minimum number of new characters at which to re-attempt page completion."
  :group 'confluence
  :type 'integer)

(defcustom confluence-max-completion-results 30
  "The maximum number of results to find when attempting completion."
  :group 'confluence
  :type 'integer)

(defcustom confluence-coding-alist nil
  "Coding systems to use for a given service ('url' -> 'coding-system').  Confluence versions < 2.8 incorrectly
managed xml encoding and used the server's platform default encoding (ignoring any specified by the xml itself).  If
you are working with an older system, you will need to configure the coding system here (default 'utf-8 is used if not
configured here)."
  :group 'confluence
  :type '(alist  :key-type string :value-type coding-system))

(defvar confluence-coding-prefix-alist (list (cons (confluence-coding-system-base 'utf-16-be) "\376\377")
                                             (cons (confluence-coding-system-base 'utf-16-le) "\377\376"))
  "Extra prefix necessary when decoding a string in a given coding system (not necessary for all coding systems).  The
empty string is used if nothing is defined here, which works for most coding systems.")

(defvar confluence-coding-bytes-per-char-alist (list (cons (confluence-coding-system-base 'utf-16-be) 2)
                                                     (cons (confluence-coding-system-base 'utf-16-le) 2))
  "Number of bytes per character for the coding system.  Assumed be be 1 or variable if not defined here, which works
for most coding systems.")

(defvar confluence-before-save-hook nil
  "List of functions to be called before saving a confluence page.")

(defvar confluence-before-revert-hook nil
  "List of functions to be called before reverting a confluence page.")

(defvar confluence-login-token-alist nil
  "AList of 'url' -> 'token' login information.")

(defvar confluence-path-history nil
  "History list of paths accessed.")

(defvar confluence-space-history nil
  "History list of spaces accessed.")

(defvar confluence-page-history nil
  "History list of pages accessed.")

(defvar confluence-search-history nil
  "History list of queries.")

(defvar confluence-label-history nil
  "History labels used.")

(defvar confluence-page-url nil
  "The url used to load the current buffer.")
(make-variable-buffer-local 'confluence-page-url)
(put 'confluence-page-url 'permanent-local t)

(defvar confluence-page-struct nil
  "The full metadata about the page in the current buffer.")
(make-variable-buffer-local 'confluence-page-struct)
(put 'confluence-page-struct 'permanent-local t)

(defvar confluence-page-id nil
  "The id of the page in the current buffer.")
(make-variable-buffer-local 'confluence-page-id)
(put 'confluence-page-id 'permanent-local t)

(defvar confluence-browse-function nil
  "The function to use for browsing links in the current buffer.")
(make-variable-buffer-local 'confluence-browse-function)
(put 'confluence-browse-function 'permanent-local t)

(defvar confluence-load-info nil
  "The information necessary to reload the page.")
(make-variable-buffer-local 'confluence-load-info)
(put 'confluence-load-info 'permanent-local t)

(defvar confluence-tag-stack nil
  "TAGs style stack support for push (\\C-xw.) and pop (\\C-xw*)")

(defconst confluence-search-types (list (cons "content" t) (cons "title" t) (cons "label" t))
  "Supported search types.")

;; these are never set directly, only defined here to make the compiler happy
(defvar confluence-coding-system nil)
(defvar confluence-coding-prefix nil)
(defvar confluence-coding-num-bytes nil)
(defvar confluence-input-url nil)
(defvar confluence-switch-url nil)
(defvar confluence-completing-read nil)

(defun confluence-login (&optional arg)
  "Logs into the current confluence url, if necessary.  With ARG, forces
re-login to the current url."
  (interactive "P")
  (if arg
      (cf-set-struct-value 'confluence-login-token-alist
                           (cf-get-url) nil))
  ;; we may need to prompt for a password while already at the minibuffer prompt, so enable recursive minibuffers
  (let ((enable-recursive-minibuffers t)
        (cur-token (cf-get-struct-value confluence-login-token-alist
                                        (cf-get-url))))
    (while (not cur-token)
      (condition-case err
          (progn
            (setq cur-token
                  (cf-rpc-execute-internal 
                   'confluence1.login
                   (read-string (format "Confluence Username [%s]: " user-login-name) nil nil user-login-name t)
                   (read-passwd "Confluence Password: ")))
            (cf-set-struct-value 'confluence-login-token-alist
                                 (cf-get-url) cur-token))
        (error (message "Failed logging in: %s" (error-message-string err)))))
    cur-token))

(defun confluence-get-page (&optional page-name space-name anchor-name)
  "Loads a confluence page for the given SPACE-NAME and PAGE-NAME
into a buffer (if not already loaded) and switches to it.
Analogous to `find-file'.  Every time you navitage to a page with
this function (or M-. `confluence-get-page-at-point'), it is
saved off into a stack (`confluence-tag-stack') that you can then
pop back out of to return back through your navigation path (with
M-* `confluence-pop-tag-stack')."
  (interactive)
  (cf-prompt-page-info nil 'page-name 'space-name)
  (cf-show-page (cf-rpc-get-page-by-name space-name page-name) nil 
                anchor-name))

(defun confluence-get-page-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the get
page call (based on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-switch-url arg)
        (confluence-input-url nil))
    (confluence-get-page)))

(defun confluence-get-page-at-point ()
  "Opens the confluence page at the current point.  If the link is a url,
opens the page using `browse-url', otherwise attempts to load it
as a confluence page.  Analogous to M-. (`find-tag')."
  (interactive)
  (if (thing-at-point-looking-at "\\[\\(\\([^|\n]*\\)[|]\\)?\\([^]\n]+\\)\\]")
      (let ((url (match-string 3)))
        (set-text-properties 0 (length url) nil url)
        (if (string-match thing-at-point-url-regexp url)
            (browse-url url)
          (if confluence-browse-function
              (funcall confluence-browse-function url)
            (cf-simple-browse-function url))))
    (if (thing-at-point 'url)
        (browse-url-at-point)
      (confluence-get-page))))

(defun confluence-get-parent-page ()
  "Loads a confluence page for the parent of the current
confluence page into a buffer (if not already loaded) and
switches to it."
  (interactive)
  (let ((parent-page-id (cf-get-struct-value confluence-page-struct "parentId" "0")))
    (if (equal parent-page-id "0")
        (message "Current page has no parent page")
      (cf-show-page (cf-rpc-get-page-by-id parent-page-id)))))

(defun confluence-get-attachment (&optional page-name space-name attachment-name)
  (interactive)
  ;; FIXME, writeme
  (error "Attachment retrieval not supported yet"))

(defun confluence-goto-anchor (&optional anchor-name)
  "Moves to the given ANCHOR-NAME in the current confluence buffer."
  (interactive)
  (if (not anchor-name)
      (setq anchor-name (cf-read-string-simple "Confluence Anchor Name: " 
                                               nil 'cf-complete-anchor-name)))
  (let ((anchor-position nil))
    (save-excursion
      (goto-char (point-min))
      (setq anchor-position
            (search-forward (concat "{anchor:" anchor-name "}") nil t)))
    (if anchor-position
        (goto-char anchor-position)
      (message "Could not find anchor %s in page..." anchor-name))))

(defun confluence-create-page (&optional page-name space-name)
  "Creates a new confluence page for the given SPACE-NAME and
PAGE-NAME and loads it into a new buffer."
  (interactive)
  (cf-prompt-page-info "New " 'page-name 'space-name)
  (let ((new-page (list (cons "content" "")))
        (parent-page-id (cf-get-parent-page-id t space-name)))
    (cf-set-struct-value 'new-page "title" page-name)
    (cf-set-struct-value 'new-page "space" space-name)
    (if parent-page-id
        (cf-set-struct-value 'new-page "parentId" parent-page-id))
    (cf-show-page (cf-rpc-save-page new-page))))

(defun confluence-create-page-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the create page call
(based on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-switch-url arg)
        (confluence-input-url nil))
    (confluence-create-page)))

(defmacro cf-destructure-tags-stack-entry (entry &rest body)
  "Destructure a tags-stack tuple.  NB this is not a hygenic
macro, it intentionally binds named variables that match the
structure of the stack entry.  The structure and the variable
bindings are:

  ((page-type confluence-input-url page-id-or-query &optional space-name) old-point)

Each stack entry can be either the result of a search query (in
which case page-type will be the symbol 'search or a page
visitation (and page-type will be 'page).  page-id-or-query will
be either a page-id or a query - depending on the type of stack
entry (page or query).  space-name will be populated when
page-type is 'search.
"
  `(destructuring-bind
       ((page-type confluence-input-url page-id-or-query &optional space-name) old-point)
       ,entry
     ,@body))

(defmacro cf-destructure-load-info (load-info &rest body)
  "Destructure a load-info tuple.  NB this is not a hygenic
macro, it intentionally binds named variables that match the
structure of the stack entry.  The structure and the variable
bindings are:

  (page-type confluence-input-url page-id-or-query &optional space-name)

Each load-info can be either the result of a search query (in
which case page-type will be the symbol 'search or a page
visitation (and page-type will be 'page).  page-id-or-query will
be either a page-id or a query - depending on the type of stack
entry (page or query).  space-name will be populated when
page-type is 'search.
"
  `(destructuring-bind
       (page-type confluence-input-url page-id-or-query &optional space-name)
       ,load-info
     ,@body))

(defun confluence-pop-tag-stack ()
  "Returns to the last previously visited space/page by popping
the tags stack."
  (interactive)
  (if (null confluence-tag-stack)
      (message "Stack is empty...")
    (cf-destructure-tags-stack-entry
        (pop confluence-tag-stack)
      (cond 
       ;; load a normal page by id
       ((eq page-type 'page)
        (cf-show-page (cf-rpc-get-page-by-id page-id-or-query) t))
       ;; run a previous search query
       ((eq page-type 'search)
        (cf-show-search-results 
         (cf-rpc-search page-id-or-query space-name)
         load-info t))
       (t
        (error "Invalid stack info")))
      (goto-char old-point))))

(defun confluence-push-tag-stack ()
  "Pushes the current page onto the visited stack if it is a confluence page."
  (interactive)
  (if confluence-load-info
      (push (list confluence-load-info (point)) confluence-tag-stack)))

(defun confluence-ediff-merge-current-page ()
  "Starts an ediff session diffing the current confluence page against the
latest version of that page saved in confluence with intent of saving the
result as the latest version of the page."
  (interactive)
  (cf-ediff-current-page t))

(defun confluence-ediff-current-page ()
  "Starts an ediff session diffing the current confluence page against the
latest version of that page saved in confluence."
  (interactive)
  (cf-ediff-current-page nil))

(defun confluence-reparent-page ()
  "Changes the parent of the current confluence page."
  (interactive)
  (let ((parent-page-id (cf-get-parent-page-id nil)))
    (if (and parent-page-id
             (not (equal parent-page-id (cf-get-struct-value confluence-page-struct "parentId"))))
        (progn
          (cf-set-struct-value 'confluence-page-struct "parentId" parent-page-id)
          (set-buffer-modified-p t)))))

(defun confluence-rename-page ()
  "Changes the name (title) of the current confluence page."
  (interactive)
  (let ((page-name (cf-prompt-page-name 
                    (cf-get-struct-value confluence-page-struct "space") 
                    "New ")))
    (if (and (cf-string-notempty page-name)
             (not (equal page-name (cf-get-struct-value confluence-page-struct "title"))))
        (progn
          (cf-set-struct-value 'confluence-page-struct "title" page-name)
          (cf-update-buffer-name)
          (set-buffer-modified-p t)))))

(defun confluence-add-label (&optional label-name)
  "Adds the label with the given name to the current confluence page."
  (interactive)
  (if confluence-page-id
      (progn
        (if (not label-name)
            (setq label-name
                  (cf-read-string-simple "New Confluence Label: " 'confluence-label-history 'cf-complete-label-name)))
        (if (cf-string-notempty label-name)
            (cf-rpc-add-label label-name confluence-page-id)))))

(defun confluence-remove-label (&optional label-name)
  "Removes the label with the given name to the current confluence page."
  (interactive)
  (if confluence-page-id
      (progn
        (if (not label-name)
          (let* ((url-show-status nil)
                 (cur-labels (cf-result-to-completion-list (cf-rpc-get-labels confluence-page-id) "name")))
            (if (= (length cur-labels) 0)
                (message "Current page has no labels...")
              (progn
                (or label-name
                    (setq label-name (cf-read-string-simple "Old Confluence Label: " 'confluence-label-history cur-labels t)))))))
        (if (cf-string-notempty label-name)
            (cf-rpc-remove-label label-name confluence-page-id)))))

(defun confluence-get-labels ()
  "Shows the labels of the current page."
  (interactive)
  (if confluence-page-id
      (let ((cur-labels (mapcar
                         '(lambda (el)
                            (cf-get-struct-value el "name"))
                         (cf-rpc-get-labels confluence-page-id))))
        (if (= (length cur-labels) 0)
            (message "Current page has no labels...")
          (message "Current Confluence Labels: %s" cur-labels)))))
  
(defun confluence-delete-page (&optional arg)
  "Deletes the current confluence page.  Asks first, unless ARG is given."
  (interactive "P")
  (if (or arg
          (yes-or-no-p  "Really delete confluence page? "))
      (progn
        (if (not confluence-page-id)
            (error "Could not delete Confluence page %s, missing page id"
                   (buffer-name)))
        (cf-rpc-execute 'confluence1.removePage confluence-page-id)
        ;; remove this page from the tag stack
        (while (assoc confluence-load-info confluence-tag-stack)
          (setq confluence-tag-stack
                (remove (assoc confluence-load-info confluence-tag-stack)
                        confluence-tag-stack)))
        (kill-buffer (current-buffer)))))

(defun confluence-search (&optional query space-name)
  "Runs a confluence search for QUERY, optionally restricting the results to
the given SPACE-NAME."
  (interactive)
  (confluence-search-by-type 'content query space-name))

(defun confluence-search-in-space (&optional query)
  "Runs a confluence search for QUERY, restricting the results to the space of
the current buffer."
  (interactive)
  (confluence-search-by-type-in-space 'content query))

(defun confluence-search-with-url (&optional arg)
  "With ARG, prompts for the confluence url to use for the search call (based
on `confluence-default-space-alist')."
  (interactive "P")
  (confluence-search-by-type-with-url arg 'content))

(defun confluence-search-by-type (&optional query-type query space-name)
  "Runs a confluence search by type (content, title, label) for QUERY, optionally restricting the results to
the given SPACE-NAME."
  (interactive)
  (or query-type
      (setq query-type (cf-read-string-simple "Confluence Search Type [content]: "
                                        nil confluence-search-types
                                        t nil "content")))
  (if (stringp query-type)
      (setq query-type (intern query-type)))
  (let ((query-prompt-prefix "")
        (query-prefix ""))
    (cond
     ((eq query-type 'title)
      (setq query-prompt-prefix "Title ")
      (setq query-prefix "title: "))
     ((eq query-type 'label)
      (setq query-prompt-prefix "Label ")
      (setq query-prefix "labelText: ")))
    (or query
        (setq query (read-string (concat "Confluence " query-prompt-prefix "Query: ") nil 
                                 'confluence-search-history nil t)))
    (setq query (concat query-prefix query))
    (cf-show-search-results (cf-rpc-search query space-name)
                            (list 'search (cf-get-url) query space-name))))

(defun confluence-search-by-type-in-space (&optional query-type query)
  "Runs a confluence search by type (content, title, label) for QUERY, restricting the results to the space of
the current buffer."
  (interactive)
  (confluence-search-by-type query-type query (cf-get-struct-value confluence-page-struct "space")))

(defun confluence-search-by-type-with-url (&optional arg query-type)
  "With ARG, prompts for the confluence url to use for the search call (based
on `confluence-default-space-alist')."
  (interactive "P")
  (let ((confluence-input-url (and arg (cf-prompt-url nil))))
    (confluence-search-by-type query-type)))

(defun confluence-preview ()
  "Preview the content in the current confluence buffer."
  (interactive)
  (if (and confluence-page-id
           confluence-page-struct)
      (let ((source-content "")
            (rendered-content nil)
            (render-buf (get-buffer-create " *Confluence-preview*")))
        ;; if the local content is modified, submit the content
        (if (buffer-modified-p)
            (progn
              (setq source-content (buffer-string))
              ;; if there are save hooks, copy the content into a temp buf and run them on the content before
              ;; submitting it
              (if confluence-before-save-hook
                  (with-current-buffer (get-buffer-create " *Confluence-decode*")
                    (erase-buffer)
                    (insert source-content)
                    (run-hooks 'confluence-before-save-hook)
                    (setq source-content (buffer-string))))))
        ;; render the current page, optionally with locally modified content
        (setq rendered-content (cf-rpc-render-page (cf-get-struct-value confluence-page-struct "space")
                                                   confluence-page-id source-content))
        ;; shove the html into a temp buffer
        (with-current-buffer render-buf
          (widen)
          (erase-buffer)
          (kill-all-local-variables)
          (insert rendered-content)
          ;; fix bug in stylesheet
          (goto-char (point-min))
          (if (re-search-forward "^\\s-*body\\s-*{[^}]*}" nil t)
              (progn
                (goto-char (match-beginning 0))
                (if (re-search-forward "text-align:\\s-*\\(center\\)" (match-end 0) t)
                    (replace-match "left" t t nil 1))))
          (set-buffer-modified-p nil))
        ;; launch browser with rendered content
        (message "Launching browser with preview content...")
        (browse-url-of-buffer render-buf))))

(defun cf-rpc-execute (method-name &rest params)
  "Executes a confluence rpc call, managing the login token and logging in if
necessary."
  (condition-case err
      (apply 'cf-rpc-execute-internal method-name (confluence-login) params)
    (error
     ;; if we get a fault with the given keywords, try the call again after a
     ;; re-login (we force re-login), otherwise, just rethrow the error
     (if (and xml-rpc-fault-string
              (string-match "\\<authenticated\\>\\|\\<expired\\>" xml-rpc-fault-string))
         (apply 'cf-rpc-execute-internal method-name (confluence-login t) params)
       (error (error-message-string err))))))

(defun cf-rpc-execute-internal (method-name &rest params)
  "Executes a raw confluence rpc call.  Handles all necessary encoding/decoding of strings."
  (setq xml-rpc-fault-string nil)
  (setq xml-rpc-fault-code   nil)
  (let* ((url-http-version "1.0")  ;; this make the xml-rpc parser happy
         (url-http-attempt-keepalives nil)
         (page-url (cf-get-url))   ;; figure out which url to use
         ;; setup the coding system to use for encoding/decoding based on the
         ;; url we will use for the call.  note, we always use the 'unix eol
         ;; conversion (no conversion) because we handle that separately,
         ;; after entities are decoded
         (tmp-coding-system (coding-system-base (cf-get-struct-value confluence-coding-alist page-url 'utf-8)))
         (confluence-coding-system (coding-system-change-eol-conversion tmp-coding-system 'unix))
         (confluence-coding-prefix (cf-get-struct-value confluence-coding-prefix-alist tmp-coding-system ""))
         (confluence-coding-num-bytes (cf-get-struct-value confluence-coding-bytes-per-char-alist
                                                           tmp-coding-system 1))
         (xml-rpc-encode-coding-system confluence-coding-system)
         (xml-rpc-encode-coding-prefix-length (length confluence-coding-prefix)))
    (if (not page-url)
        (error "No confluence url configured"))
    (condition-case err
        (let ((rpc-result (cf-url-decode-entities-in-value (apply 'xml-rpc-method-call page-url method-name params))))
          ;; clear any url messages before returning
          (message nil)
          rpc-result)
      (error
       ;; decode the fault string and the error message (often includes the
       ;; fault string) and then rethrow the error
       (if xml-rpc-fault-string
           (setq xml-rpc-fault-string (cf-url-decode-entities-in-value 
                                       xml-rpc-fault-string)))
       (error (cf-url-decode-entities-in-value (error-message-string err)))))))

(defun cf-rpc-get-page-by-name (space-name page-name)
  "Executes a confluence 'getPage' rpc call with space and page names."
  (cf-rpc-execute 'confluence1.getPage space-name page-name))

(defun cf-rpc-get-page-by-id (page-id)
  "Executes a confluence 'getPage' rpc call with a page id."
  (cf-rpc-execute 'confluence1.getPage page-id))

(defun cf-rpc-search (query space-name &optional max-results)
  "Executes a confluence 'search' rpc call, optionally restricted by the given
SPACE-NAME."
  (let ((params (list (cons "type" "page"))))
    (if (cf-string-notempty space-name)
        (cf-set-struct-value 'params "spaceKey" space-name))
    (cf-rpc-execute 'confluence1.search query
                    params (or max-results confluence-search-max-results))))

(defun cf-rpc-save-page (page-struct)
  "Executes a confluence 'storePage' rpc call with a page struct."
  (cf-rpc-execute 'confluence1.storePage page-struct))

(defun cf-rpc-get-spaces ()
  "Executes a confluence 'getSpaces' rpc call."
  (cf-rpc-execute 'confluence1.getSpaces))

(defun cf-rpc-get-space (space-name)
  "Executes a confluence 'getSpace' rpc call with space name."
  (cf-rpc-execute 'confluence1.getSpace space-name))

(defun cf-rpc-get-labels (obj-id)
  "Executes a confluence 'getLabelsById' rpc call with object id."
  (cf-rpc-execute 'confluence1.getLabelsById obj-id))

(defun cf-rpc-get-recent-labels (max-results)
  "Executes a confluence 'getRecentlyUsedLabels' rpc call with the given max results."
  (cf-rpc-execute 'confluence1.getRecentlyUsedLabels max-results))

(defun cf-rpc-add-label (label-name obj-id)
  "Executes a confluence 'addLabelByName' rpc call with label name and object id."
  (cf-rpc-execute 'confluence1.addLabelByName label-name obj-id))

(defun cf-rpc-remove-label (label-name obj-id)
  "Executes a confluence 'removeLabelByName' rpc call with label name and object id."
  (cf-rpc-execute 'confluence1.removeLabelByName label-name obj-id))

(defun cf-rpc-render-page (space-name page-id &optional content)
  "Executes a confluence 'renderContent' rpc call with space and page id and optional content."
  (cf-rpc-execute 'confluence1.renderContent space-name page-id (or content "")))

(defun cf-ediff-current-page (update-cur-version)
  "Starts an ediff session for the current confluence page, optionally
updating the saved metadata to the latest version."
  (if (not confluence-page-id)
      (error "Could not diff Confluence page %s, missing page id"
             (buffer-name)))
  (let ((rev-buf)
        (cur-buf (current-buffer))
        (rev-page (cf-rpc-get-page-by-id confluence-page-id)))
    (setq rev-buf
          (get-buffer-create (format "%s.~%s~" (buffer-name cur-buf) (cf-get-struct-value rev-page "version" 0))))
    ;; create read-only temp buffer w/ the latest page data
    (with-current-buffer rev-buf
      (cf-insert-page rev-page)
      (toggle-read-only 1))
    ;; optionally update the metadata in the current buffer (and update the
    ;; buffer name in case the page title changed)
    (if update-cur-version
        (progn
          (setq confluence-page-struct 
                (cf-set-struct-value-copy rev-page "content" ""))
          (cf-update-buffer-name)))
    (ediff-buffers cur-buf rev-buf nil 'confluence-diff)))

(defun cf-save-page ()
  "Saves the current confluence page and updates the buffer with the latest
page."
  (if (not confluence-page-id)
      (error "Could not save Confluence page %s, missing page id"
             (buffer-name)))
  (widen)
  (run-hooks 'confluence-before-save-hook)
  (cf-insert-page (cf-rpc-save-page 
                   (cf-set-struct-value-copy confluence-page-struct 
                                             "content" (buffer-string))) 
                  nil nil t)
  t)

(defun cf-revert-page (&optional arg noconfirm)
  "Reverts the current buffer to the latest version of the current confluence
page."
  (if (and confluence-load-info
           (or noconfirm
               (yes-or-no-p "Revert confluence page? ")))
      (progn
        ;; use the load-info to reload the page, so we can reload normal pages
        ;; and search pages
        (cf-destructure-load-info confluence-load-info
          (cond 
           ;; reload normal page data
           ((eq page-type 'page)
            (cf-insert-page (cf-rpc-get-page-by-id page-id-or-query) confluence-load-info)
            (cf-update-buffer-name))
           ;; reload search page data
           ((eq page-type 'search)
            (cf-insert-search-results 
             (cf-rpc-search page-id-or-query space-name)
             confluence-load-info))
           (t
            (error "Invalid load info")))))))

(defun cf-show-page (full-page &optional no-push anchor-name)
  "Does the work of finding or creating a buffer for the given confluence page
and loading the data if necessary."
  (if (not no-push)
      (confluence-push-tag-stack))
  ;; note, we save the current url as confluence-input-url in case the buffer
  ;; has a different value locally from a previous searcg (this value will
  ;; override it)
  (let* ((confluence-input-url (cf-get-url))
         (load-info (list 'page confluence-input-url (cf-get-struct-value full-page "id")))
         (page-buffer (get-buffer-create (cf-format-buffer-name
                                          (cf-get-struct-value full-page "title")
                                          (cf-get-struct-value full-page "space")))))
    ;; only insert the data if the buffer is new, otherwise just show current
    ;; data
    (with-current-buffer page-buffer
      (if (not (equal confluence-load-info load-info))
          (progn
            (cf-insert-page full-page load-info)
            (goto-char (point-min))))
      (if anchor-name
          (confluence-goto-anchor anchor-name)))
    (switch-to-buffer page-buffer)))

(defun cf-insert-page (full-page &optional load-info browse-function keep-undo)
  "Does the work of loading confluence page data into the current buffer.  If
KEEP-UNDO, the current undo state will not be erased.  The LOAD-INFO is the 
information necessary to reload the page (if nil, normal page info is used)."
  ;; if this is an old buffer (already has confluence-mode), run
  ;; revert hooks before writing new data
  (if (eq major-mode 'confluence-mode)
      (run-hooks 'confluence-before-revert-hook))
  (let ((old-point (point))
        (was-read-only buffer-read-only))
    (if was-read-only
        (toggle-read-only))
    ;; save/update various page metadata
    (setq confluence-page-struct full-page)
    (setq confluence-page-url (cf-get-url))
    (setq confluence-page-id (cf-get-struct-value confluence-page-struct "id"))
    (setq confluence-load-info 
          (or load-info
              (list 'page confluence-page-url confluence-page-id)))
    (if browse-function
        (setq confluence-browse-function browse-function))
    ;; don't save the buffer edits on the undo list (we might keep it)
    (let ((buffer-undo-list t)
          (inhibit-read-only t))
      (widen)
      (erase-buffer)
      ;; actually insert the new page contents
      (insert (cf-get-struct-value confluence-page-struct "content" ""))
      (goto-char old-point))
    ;; remove the contents from the page metadata
    (cf-set-struct-value 'confluence-page-struct "content" "")
    ;; restore/setup buffer state
    (set-buffer-modified-p nil)
    (or keep-undo
        (eq buffer-undo-list t)
        (setq buffer-undo-list nil))
    (confluence-mode)
    (if was-read-only
        (toggle-read-only 1))))

(defun cf-show-search-results (search-results load-info &optional no-push)
  "Does the work of finding or creating a buffer for the given confluence
search results and loading the data into that page."
  (if (not no-push)
      (confluence-push-tag-stack))
  ;; note, we save the current url as confluence-input-url in case the buffer
  ;; has a different value locally from a previous searcg (this value will
  ;; override it)
  (let ((confluence-input-url (cf-get-url))
        (search-buffer (get-buffer-create "*Confluence Search Results*")))
    (with-current-buffer search-buffer
      ;; only reload the page if this is a new search, otherwise keep current
      ;; data
      (if (not (equal confluence-load-info load-info))
          (progn
            (cf-insert-search-results search-results load-info)
            (goto-char (point-min))
            (toggle-read-only 1))))  ;; always make search results read-only
    (switch-to-buffer search-buffer)))

(defun cf-insert-search-results (search-results load-info)
  "Does the work of loading confluence search data into the current buffer."
  (let ((search-page (list (cons "title" "Confluence Search Results"))))
    ;; turn the search results into a wiki-like page
    (with-temp-buffer
      (insert "h1. Confluence Search Results for '" (nth 2 load-info) "'\n\n")
      (dolist (search-result search-results)
        (insert (format "[%s|%s]\n"
                        (cf-get-struct-value search-result "title")
                        (cf-get-struct-value search-result "id")))
        (let ((excerpt (cf-get-struct-value search-result "excerpt")))
          (if (cf-string-notempty excerpt)
              (insert excerpt "\n")))
        (insert "\n"))
      (cf-set-struct-value 'search-page "content" (buffer-string)))
    ;; install a special browse-function for loading the search urls (which
    ;; use page ids)
    (cf-insert-page search-page load-info 'cf-search-browse-function)))

(defun cf-search-browse-function (url)
  "Browse function used in search buffers (the links are page ids)."
  (cf-show-page (cf-rpc-get-page-by-id url)))

(defun cf-simple-browse-function (url)
  "Simple browse function used in page buffers."
  (let ((space-name (cf-get-struct-value confluence-page-struct "space"))
        (page-name url)
        (anchor-name nil)
        (attachment-name nil)
        (explicit-space nil))
    ;; split "space:page" links
    (if (string-match "^\\([^:\n]*\\)[:]\\(.*\\)$" page-name)
        (progn
          (setq explicit-space t)
          (setq space-name (match-string 1 page-name))
          (setq page-name (match-string 2 page-name))))
    ;; strip off any trailing "|link tip"
    (if (string-match "^\\([^|\n]*\\)[|]\\(.*\\)$" page-name)
        (setq page-name (match-string 1 page-name)))
    ;; get '^' (attachment) or '#' (anchor)
    (if (string-match "^\\(.*?\\)\\([#^]\\)\\(.*\\)$" page-name)
        (progn
          (if (equal (match-string 2 page-name) "^")
              (setq attachment-name (match-string 3 page-name))
            (setq anchor-name (match-string 3 page-name)))
          (setq page-name (match-string 1 page-name))))
    (cond
     ;; open an attachment
     ((cf-string-notempty attachment-name)
      (if (cf-string-empty page-name)
          (setq page-name (cf-get-struct-value 
                           confluence-page-struct "title")))
      (confluence-get-attachment page-name space-name attachment-name))
     ;; goto anchor in this page
     ((and (cf-string-notempty anchor-name)
           (cf-string-empty page-name))
      (confluence-goto-anchor anchor-name))
     ;; goto space "home" page
     ((and explicit-space
           (cf-string-notempty space-name)
           (cf-string-empty page-name))
      (cf-show-page
       (cf-rpc-get-page-by-id (cf-get-struct-value 
                               (cf-rpc-get-space space-name) "homePage"))))
     ;; goto user profile page (load like space "home" page)
     ((and (not explicit-space)
           (string-match "^[~].+$" page-name))
      (cf-show-page
       (cf-rpc-get-page-by-id (cf-get-struct-value 
                               (cf-rpc-get-space page-name) "homePage"))))
     (t
      (confluence-get-page page-name space-name anchor-name)))))

(defun cf-get-parent-page-id (try-current-page &optional space-name)
  "Gets a confluence parent page id, optionally using the one in the current
buffer."
  ;; if current page is a confluence page and try-current-page, ask if use
  ;; wants to use it as the parent page
  (if (and try-current-page
           confluence-page-id
           (yes-or-no-p "Use current confluence page for parent? "))
      confluence-page-id
    ;; otherwise, prompt for parent page
    (let ((parent-space-name (or space-name (cf-get-struct-value confluence-page-struct "space")))
          (parent-page-name nil))
      (cf-prompt-page-info "Parent " 'parent-page-name 'parent-space-name)
      (if (and (cf-string-notempty parent-space-name)
               (cf-string-notempty parent-page-name))
          (cf-get-struct-value (cf-rpc-get-page-by-name parent-space-name parent-page-name) "id")
        nil))))

(defun cf-prompt-page-info (prompt-prefix page-name-var space-name-var)
  "Prompts for page info using the appropriate input function and sets the given vars appropriately."
  (let ((result-list
         (funcall confluence-prompt-page-function prompt-prefix
                  (symbol-value page-name-var) (symbol-value space-name-var))))
    (set page-name-var (nth 0 result-list))
    (set space-name-var (nth 1 result-list))))

(defun cf-prompt-page-by-component (prompt-prefix page-name space-name)
  "Builds a list of (page-name space-name <url>) by prompting the user for each.  Suitable for use with
`confluence-prompt-page-function'."
  (let ((result-list nil))
    ;; prompt for url if confluence-switch-url is specified
    (if (and confluence-switch-url (not confluence-input-url))
        (setq confluence-input-url (cf-prompt-url prompt-prefix)))
    ;; now, prompt for space and page if not already defined by caller
    (if (not space-name)
        (setq space-name (cf-prompt-space-name prompt-prefix)))
    (push space-name result-list)
    (push (or page-name
              (cf-prompt-page-name space-name prompt-prefix)) result-list)
    result-list))

(defun cf-prompt-page-by-path (prompt-prefix page-name space-name)
  "Builds a list of (page-name space-name <url>) by prompting the user for each (where page and space name are
specified as one path).  Suitable for use with `confluence-prompt-page-function'."
  (let ((result-list nil)
        (page-path nil))
    ;; prompt for url if confluence-switch-url is specified
    (if (and confluence-switch-url (not confluence-input-url))
        (setq confluence-input-url (cf-prompt-url prompt-prefix)))
    ;; now, prompt for space/page if both are not already defined by caller
    (if (and page-name space-name)
        (setq result-list (cons page-name (cons space-name result-list)))
      (progn
        (setq page-path (cf-prompt-path prompt-prefix page-name space-name))
        ;; split path into space and page
        (push (cf-get-space-name-from-path page-path) result-list)
        (push (cf-get-page-name-from-path page-path) result-list)))
    result-list))

(defun cf-prompt-url (&optional prompt-prefix)
  "Prompts for a confluence url."
  (let ((temp-url-hist (and confluence-default-space-alist
                            (mapcar 'car confluence-default-space-alist))))
    (read-string (concat (or prompt-prefix "") "Confluence Url: ") nil 'temp-url-hist (cf-get-url) t)))

(defun cf-prompt-space-name (&optional prompt-prefix)
  "Prompts for a confluence space name."
  (let* ((def-space (cf-get-default-space))
         (space-prompt (if def-space
                           (format "Confluence Space [%s]: " def-space)
                         "Confluence Space: ")))
    (cf-read-string prompt-prefix space-prompt 'confluence-space-history
                    (cf-get-url)
                    'cf-complete-space-name t
                    (cf-get-struct-value confluence-page-struct "space") 
                    def-space)))

(defun cf-prompt-page-name (space-name &optional prompt-prefix)
  "Prompts for a confluence page name."
  (cf-read-string prompt-prefix "Confluence Page Name: " 
                  'confluence-page-history (cons space-name (cf-get-url))
                  'cf-complete-page-name nil))

(defun cf-prompt-path (prompt-prefix page-name space-name)
  "Prompts for a confluence page path."
  (cf-read-string prompt-prefix "Confluence Space/PageName: "
                  'confluence-path-history (cf-get-url)
                  'cf-complete-page-path nil
                  (if space-name (concat space-name "/") nil)))

(defun cf-read-string (prompt-prefix prompt hist-alist-var hist-key 
                       comp-func-or-table &optional
                       require-match init-val def-val)
  "Prompt for a string using the given prompt info and history alist."
  ;; we actually use the history var as an alist of history vars so we can
  ;; have different histories in different contexts (e.g. separate space
  ;; histories for each url and separate page histories for each space)
  (let ((hist-list (cf-get-struct-value (symbol-value hist-alist-var) 
                                        hist-key))
        (result-string nil))
    (setq result-string
          (cf-read-string-simple (concat (or prompt-prefix "") prompt)
                                 'hist-list comp-func-or-table
                                 require-match init-val def-val))
    ;; put the new history list back into the alist
    (cf-set-struct-value hist-alist-var hist-key hist-list)
    result-string))

(defun cf-read-string-simple (prompt hist-list-var comp-func-or-table
                              &optional require-match init-val def-val)
  "Prompt for a string using the given prompt info and history list."
  (let ((current-completions nil)
        (current-other-completions nil)
        (last-comp-str nil)
        (url-show-status nil)
        (completion-buffer (or (and (boundp 'completion-buffer)
                                    completion-buffer)
                               (current-buffer)))
        (confluence-completing-read t))
    (completing-read prompt comp-func-or-table
                     nil require-match init-val hist-list-var def-val t)))

(defun cf-minibuffer-setup ()
  "Minibuffer setup hook which changes some keybindings for confluence completion."
  (if confluence-completing-read
      ;; don't do completion when spaces are entered (just confusing)
      (local-set-key " " 'self-insert-command)))

(add-hook 'minibuffer-setup-hook 'cf-minibuffer-setup t)

(defun cf-get-space-name-from-path (page-path)
  "Parses the space name from the given PAGE-PATH."
  (if (string-match "\\([^/]+\\)[/]\\(.*\\)" page-path)
      (match-string 1 page-path)
    (cf-get-default-space)))

(defun cf-get-page-name-from-path (page-path)
  "Parses the page name from the given PAGE-PATH."
  (if (string-match "\\([^/]+\\)[/]\\(.*\\)" page-path)
      (match-string 2 page-path)
    page-path))

(defun cf-get-struct-value (struct key &optional default-value)
  "Gets a STRUCT value for the given KEY from the given struct, returning the
given DEFAULT-VALUE if not found."
  (or (and struct
           (cdr (assoc key struct)))
      default-value))

(defun cf-set-struct-value-copy (struct key value)
  "Copies the given STRUCT, sets the given KEY to the given VALUE and returns
the new STRUCT."
  (let ((temp-struct (copy-alist struct)))
    (cf-set-struct-value 'temp-struct key value)
    temp-struct))

(defun cf-set-struct-value (struct-var key value)
  "Sets (or adds) the given KEY to the given VALUE in the struct named by the
given STRUCT-VAR."
  (let ((cur-assoc (assoc key (symbol-value struct-var))))
    (if cur-assoc
        (setcdr cur-assoc value)
      (add-to-list struct-var (cons key value) t))))

(defun cf-result-to-completion-list (result-list key)
  "Translates the rpc result list into a list suitable for completion."
  (mapcar
   '(lambda (el)
      (cons (cf-get-struct-value el key) t))
   result-list))

(defun cf-complete-space-name (comp-str pred comp-flag)
  "Completion function for confluence spaces."
  (if (not current-other-completions)
      (with-current-buffer completion-buffer
        (setq current-other-completions (cf-result-to-completion-list (cf-rpc-get-spaces) "key"))))
  (cond
   ((not comp-flag)
    (or (try-completion comp-str current-other-completions pred) comp-str))
   ((eq comp-flag t)
    (or (all-completions comp-str current-other-completions pred) (list comp-str)))
   ((eq comp-flag 'lambda)
    (and (assoc comp-str current-other-completions) t))))

(defun cf-complete-page-name (comp-str pred comp-flag)
  "Completion function for confluence pages."

  ;; clear previous completion info if beginning of current string does not match previous string
  (let ((tmp-comp-str (replace-regexp-in-string "^\\(\\s-\\|\\W\\)*\\(.*?\\)\\(\\s-\\|\\W\\)*$"
                                                "\\2" comp-str t))
        (old-current-completions nil))
    (if (and last-comp-str
             (not (eq t (compare-strings last-comp-str 0 (length last-comp-str)
                                         tmp-comp-str 0 (length last-comp-str) t))))
        (progn
          (setq last-comp-str nil)
          (setq current-completions nil))
      ;; if the new string is over the repeat search threshold, clear previous search results
      (if (and last-comp-str
               (<= (+ (length last-comp-str) confluence-min-page-repeat-completion-length)
                   (length tmp-comp-str)))
          (progn
            (setq old-current-completions current-completions)
            (setq current-completions nil))))
    
  ;; retrieve page completions if necessary
  (if (and (>= confluence-min-page-completion-length 0)
           (not current-completions)
           (>= (length tmp-comp-str) confluence-min-page-completion-length))
      (let ((title-query
             (replace-regexp-in-string "\\(\\W\\)" "\\\\\\&" tmp-comp-str t)))
        ;; the search syntax is a little flaky, sometimes quotes are good, sometimes not...
        (setq title-query
              (concat "title: "
                      (if (string-match "\\s-" title-query)
                          (concat title-query "*")
                        (concat "\"" title-query "*\""))))
        (setq last-comp-str tmp-comp-str)
        (with-current-buffer completion-buffer
          (setq current-completions (cf-result-to-completion-list
                                     (cf-rpc-search title-query space-name confluence-max-completion-results)
                                     "title")))
        ;; the query results are flaky, if we had results before and none now, reuse the old list
        (if (and (= (length current-completions) 0)
                 old-current-completions)
            (setq current-completions old-current-completions))
        )))
  
  (cond
   ((not comp-flag)
    (or (try-completion comp-str current-completions pred) comp-str))
   ((eq comp-flag t)
    (or (all-completions comp-str current-completions pred) (list comp-str)))
   ((eq comp-flag 'lambda)
    (and (assoc comp-str current-completions) t))))

(defun cf-complete-page-path (comp-str pred comp-flag)
  "Completion function for confluence page paths."
  (let ((space-name comp-str)
        (page-name nil))
    (if (string-match "\\([^/]+\\)[/]\\(.*\\)" comp-str)
        (progn
          (setq space-name (match-string 1 comp-str))
          (setq page-name (match-string 2 comp-str))))
    (if (not page-name)
        (cf-complete-space-name comp-str pred comp-flag)
      (let ((page-comp-result (cf-complete-page-name page-name pred comp-flag)))
        (cond
         ((stringp page-comp-result)
          (concat space-name "/" page-comp-result))
         ((listp page-comp-result)
          (mapcar
           '(lambda (el)
              (concat space-name "/" el)) page-comp-result))
         (t page-comp-result))))))

(defun cf-complete-label-name (comp-str pred comp-flag)
  "Completion function for confluence labels."
  (if (not current-completions)
      (with-current-buffer completion-buffer
        (setq current-completions (cf-result-to-completion-list (cf-rpc-get-recent-labels
                                                                 confluence-max-completion-results) "name"))))
  (cond
   ((not comp-flag)
    (or (try-completion comp-str current-completions pred) comp-str))
   ((eq comp-flag t)
    (or (all-completions comp-str current-completions pred) (list comp-str)))
   ((eq comp-flag 'lambda)
    (and (assoc comp-str current-completions) t))))

(defun cf-complete-anchor-name (comp-str pred comp-flag)
  "Completion function for confluence anchors."
  (if (not current-completions)
      (save-excursion
        (with-current-buffer completion-buffer
          (goto-char (point-min))
          (while (re-search-forward "{anchor:\\([^{}\n]+\\)}" nil t)
            (push (cons (match-string 1) t) current-completions)))))
  (cond
   ((not comp-flag)
    (or (try-completion comp-str current-completions pred) comp-str))
   ((eq comp-flag t)
    (or (all-completions comp-str current-completions pred) (list comp-str)))
   ((eq comp-flag 'lambda)
    (and (assoc comp-str current-completions) t))))

(defun cf-update-buffer-name ()
  "Sets the buffer name based on the buffer info if it is a page buffer."
  (let ((page-name (cf-get-struct-value confluence-page-struct "title"))
        (space-name (cf-get-struct-value confluence-page-struct "space")))
    ;; only update if the current buffer has title and space (this method will
    ;; do nothing on search pages)
    (if (and (cf-string-notempty page-name)
             (cf-string-notempty space-name))
        (rename-buffer (cf-format-buffer-name page-name space-name)))))

(defun cf-format-buffer-name (page-name space-name)
  "Formats the name of the buffer given the page and space name."
  (format "%s<%s>" page-name space-name))

(defun cf-get-url ()
  "Gets the confluence url to use for the current operation."
  ;; get the relevant url, by precedence:
  ;; - input url - optionally defined by current operation
  ;; - page url - if current buffer is confluence buffer, this will be the url
  ;;              from which it was loaded
  ;; - confluence-url user configured default
  (or confluence-input-url confluence-page-url confluence-url))

(defun cf-get-default-space ()
  "Gets the default confluence space to use for the current operation."
  (cf-get-struct-value confluence-default-space-alist (cf-get-url)))

(defun cf-string-notempty (str)
  "Returns t if the given string is not empty."
  (> (length str) 0))

(defun cf-string-empty (str)
  "Returns t if the given string is empty."
  (= (length str) 0))

(defun cf-url-decode-entities-in-value (value)
  "Decodes XML entities in the given value, which may be a struct, list or
something else."
  (cond 
   ((listp value)
    (dolist (struct-val value)
      (setcdr struct-val 
              (cf-url-decode-entities-in-value (cdr struct-val)))))
   ((stringp value)
    (setq value (cf-url-decode-entities-in-string value))))
  value)

(defun cf-url-decode-entities-in-string (string)
  "Convert XML entities to string values:
    &amp;    ==>  &
    &lt;     ==>  <
    &gt;     ==>  >
    &quot;   ==>  \"
    &#[0-9]+ ==>  <appropriate char>"
  (if (and (stringp string)
           (string-match "[&\r]" string))
      (save-excursion
	(set-buffer (get-buffer-create " *Confluence-decode*"))
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(insert string)
	(goto-char (point-min))
        (while (re-search-forward "&\\([^;\n]+\\);" nil t)
          (let ((ent-str (match-string 1))
                (ent-point (match-beginning 1)))
            (replace-match 
             (cond
              ;; simple xml entities
              ((cdr-safe (assoc ent-str
                                '(("quot" . "\"")
                                  ("amp" . "&")
                                  ("lt" . "<")
                                  ("gt" . ">")))))
              ;; decimal number character entities
              ((save-match-data
                 (and (string-match "^#\\([0-9]+\\)$" ent-str)
                      (cf-number-entity-to-string (string-to-number (match-string 1 ent-str))))))
              ;; hexidecimal number character entities
              ((save-match-data
                 (and (string-match "^#x\\([0-9A-Fa-f]+\\)$" ent-str)
                      (cf-number-entity-to-string (string-to-number (match-string 1 ent-str) 16)))))
              ;; unknown entity
              (t (concat "&" ent-str ";")))
             t t)
            (goto-char ent-point)))

	(goto-char (point-min))
        ;; always convert to unix newlines
        (while (re-search-forward "\r\n" nil t)
          (replace-match "\n" t t))
	(buffer-string))
    string))

(defun cf-number-entity-to-string (num)
  "Convert an xml number entity to the appropriate character using the current `confluence-coding-system' (which is
set by `cf-rpc-execute-internal')."
  ;; split the number into bytes and put these all into char-list
  (let ((char-list nil))
    (push (logand num 255) char-list)
    (setq num (lsh num -8))
    (while (/= 0 num)
      (push (logand num 255) char-list)
      (setq num (lsh num -8)))
    ;; if the currenting coding system has fixed lenth chars, pad the
    ;; character list as necessary
    (while (< (length char-list) confluence-coding-num-bytes)
      (push 0 char-list))
    ;; finally, turn the char list into a string and decode it (some encodings
    ;; require a prefix, so slap that on here as well)
    (decode-coding-string (concat confluence-coding-prefix (apply 'string char-list)) confluence-coding-system t)))

(defadvice url-display-percentage (around url-display-percentage-quiet
                                          activate compile preactivate)
  "Make `url-display-percentage' respect `url-show-status'."
  (if url-show-status
      ad-do-it))

;;;;;;;;;;;;;;;;;;;
;;; Confluence mode

(defvar confluence-code-face 'confluence-code-face)

(defface confluence-code-face
  '((((class color) (background dark))
     (:foreground "dim gray" :bold t))
    (((class color) (background light))
     (:foreground "dim gray"))
    (t (:bold t)))
  "Font Lock Mode face used for code in confluence pages.")

(defvar confluence-panel-face 'confluence-panel-face)

(defface confluence-panel-face
  '((((class color) (background dark))
     (:background "LightGray"))
    (((class color) (background light))
     (:background "LightGray"))
    (t nil))
  "Font Lock Mode face used for panel in confluence pages.")


(defconst confluence-font-lock-keywords-1
  (list
  
   '("{\\([^{}:\n]+:?\\)[^{}\n]*}"
     (1 'font-lock-constant-face))
  
   '("{[^{}\n]+[:|]title=\\([^}|\n]+\\)[^{}\n]*}"
     (1 'bold append))
  
   '("{warning\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){warning}"
     (1 'font-lock-warning-face prepend))
   '("{note\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){note}"
     (1 'font-lock-minor-warning-face prepend))
   '("{info\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){info}"
     (1 'font-lock-doc-face prepend))
   '("{tip\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){tip}"
     (1 'font-lock-comment-face prepend))
  
   ;; bold
   '("[ ][*]\\([^*\n]+\\)[*][ ]"
     (1 'bold))
   
   ;; code
   '("{{\\([^}\n]+\\)}}"
     (1 'confluence-code-face t))
   
   ;; italics/emphasised
   '("[ ]_\\([^_\n]+\\)_[ ]"
     (1 'italic prepend))

   ;; underline
   '("[ ][+]\\([^+\n]+\\)[+][ ]"
     (1 'underline prepend))

   ;; strike-through
   '("[ ][-]\\([^-\n]+\\)[-][ ]"
     (1 '(:strike-through t) prepend))

   ;; headings
   '("^h1[.] \\(.*\\)$"
     (1 '(bold underline) prepend))
   '("^h2[.] \\(.*\\)$"
     (1 '(bold italic underline) prepend))
   '("^h3[.] \\(.*\\)$"
     (1 '(italic underline) prepend))
   '("^h[4-9][.] \\(.*\\)$"
     (1 'underline prepend))

   ;; bullet points
   '("^\\([*#]+\\)[ ]"
     (1 'font-lock-constant-face))
   
   ;; links
   '("\\(\\[\\)\\([^|\n]*\\)[|]\\([^]]+\\)\\(\\]\\)"
     (1 'font-lock-constant-face)
     (2 'font-lock-string-face)
     (3 'underline)
     (4 'font-lock-constant-face))
   '("\\(\\[\\)\\([^]|\n]+\\)\\(\\]\\)"
     (1 'font-lock-constant-face)
     (2 '(font-lock-string-face underline))
     (3 'font-lock-constant-face))
   '("{anchor:\\([^{}\n]+\\)}"
     (1 'font-lock-string-face))

   ;; images, embedded content
   '("\\([!]\\)\\([^!\n]+\\)\\([!]\\)"
     (1 'font-lock-constant-face)
     (2 'font-lock-reference-face)
     (3 'font-lock-constant-face))
   
   ;; tables
   '("[|]\\{2\\}\\([^|\n]+\\)"
     (1 'bold))
   '("\\([|]\\{1,2\\}\\)"
     (1 'font-lock-constant-face))
   )
  
  "Basic level highlighting for confluence mode.")

(defconst confluence-font-lock-keywords-2
  (append confluence-font-lock-keywords-1
          (list
  
           ;; code/preformatted blocks
           '("{noformat\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){noformat}"
             (1 'confluence-code-face t))
           '("{code\\(?:[:][^}\n]*\\)?}\\(\\(.\\|[\n]\\)*?\\){code}"
             (1 'confluence-code-face t))

           ;; panels
           '("{panel\\(?:[:][^}\n]*\\)?}\\(?:\\s-*[\r]?[\n]\\)?\\(\\(.\\|[\n]\\)*?\\){panel}"
             (1 'confluence-panel-face append))
           ))
  "Gaudy level highlighting for confluence mode.")

(defvar confluence-font-lock-keywords confluence-font-lock-keywords-1
  "Default expressions to highlight in Confluence modes.")


(define-derived-mode confluence-mode text-mode "Confluence"
  "Set major mode for editing Confluence Wiki pages."
  (turn-off-auto-fill)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'cf-revert-page)
  ;; FIXME, should we support local backup files?
  (make-local-variable 'make-backup-files)
  (setq make-backup-files nil)
  (add-hook 'write-contents-hooks 'cf-save-page)
  ;; we set this to some nonsense so save-buffer works
  (setq buffer-file-name (expand-file-name (concat "." (buffer-name)) "~/"))
  (setq font-lock-defaults
        '((confluence-font-lock-keywords confluence-font-lock-keywords-1
                                         confluence-font-lock-keywords-2)
          nil nil nil nil))
)

(defvar confluence-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'confluence-get-page)
    (define-key map "c" 'confluence-create-page)
    (define-key map "=" 'confluence-ediff-current-page)
    (define-key map "m" 'confluence-ediff-merge-current-page)
    (define-key map "p" 'confluence-get-parent-page)
    (define-key map "r" 'confluence-rename-page)
    (define-key map "s" 'confluence-search)
    (define-key map "." 'confluence-get-page-at-point)
    (define-key map "*" 'confluence-pop-tag-stack)
    (define-key map "v" 'confluence-preview)
    map)
  "Keybinding prefix map which can be bound for common functions in confluence mode.")

(defun confluence-newline-and-indent ()
  "Inserts a newline and indents using the previous indentation.
Supports lists, tables, and headers."
  (interactive)
  (let ((indentation nil)
        (limit nil))
    ;; find the beginning of the previous line, skipping "soft" newlines if
    ;; "hard" newlines are being used (like in longlines mode)
    (save-excursion
      (while (and (search-backward "\n" nil 'silent)
                  use-hard-newlines
                  (not (get-text-property (match-beginning 0) 'hard))))
      (setq limit (point)))
    ;; find the indentation of the previous line
    (save-excursion
      (if (re-search-backward "^\\(?:\\(?:\\(?:[*#]+\\|h[0-9][.]\\)[ \t]+\\)\\|[|]+\\)" limit t)
          (setq indentation (match-string 0))))
    (newline)
    (if indentation
        (insert indentation))))

(defun confluence-list-indent-dwim (&optional arg)
  "Increases the list indentationn on the current line by 1 bullet.  With ARG decreases by 1 bullet."
  (interactive "P")
  (let ((indent-arg (if arg -1 1)))
    (if (and mark-active transient-mark-mode)
        (let ((beg (min (point) (mark)))
              (end (max (point) (mark)))
              (tmp-point nil))
          (save-excursion
            (goto-char end)
            (if (bolp)
                (forward-line -1))
            (setq tmp-point (line-beginning-position))
            (confluence-modify-list-indent indent-arg)
            (while (and (forward-line -1)
                        (not (equal (line-beginning-position) tmp-point))
                        (>= (line-end-position) beg))
              (setq tmp-point (line-beginning-position))
              (confluence-modify-list-indent indent-arg))
          ))
    (confluence-modify-list-indent indent-arg))))

(defun confluence-modify-list-indent (depth)
  "Updates the list indentation on the current line, adding DEPTH bullets if DEPTH is positive or removing DEPTH
bullets if DEPTH is negative (does nothing if DEPTH is 0)."
  (interactive "nList Depth Change: ")
  (save-excursion
    (beginning-of-line)
    (cond
     ((> depth 0)
      (let ((indent-str (concat (make-string depth ?*) " ")))
        (if (re-search-forward "\\=\\([*#]+\\)" (line-end-position) t)
            (setq indent-str (make-string depth (elt (substring (match-string 1) -1) 0))))
        (insert-before-markers indent-str)))
     ((< depth 0)
      (let ((tmp-point (point))
            (indent-str ""))
        (if (re-search-forward "\\=\\([*#]+\\)" (line-end-position) t)
            (progn 
              (setq indent-str (match-string 1))
              (setq indent-str
                    (if (< (abs depth) (length indent-str))
                        (substring indent-str 0 depth)
                      ""))))
        (delete-region tmp-point (point))
        (insert-before-markers indent-str))))))
  
(provide 'confluence)
;;; confluence.el ends here
