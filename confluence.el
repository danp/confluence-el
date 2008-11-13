;;; confluence.el --- Extensions for interacting with the Confluence
;;; Wiki.

;; Copyright (C) 2006
;; Kyle R. Burton <kyle.burton@gamil.com>

;; Authors: Kyle R. BUrton <kyle.burton@gmail.com>
;; Keywords: confluence, wiki, jscheme, xmlrpc

;; Special Thanks To:
;; Alan Salewski asalweski@hmsonline.com

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This is a set of emacs functions for interacting with the
;; Confluence wiki.  For more infomation about Confluence see:
;;
;;    http://www.atlassian.com/software/confluence/
;;

;;; How to install:

;; Sorry for starting out with an appology section, but so be it.  I
;; tried finding and using an xmlrpc emacs lisp library, but could not
;; find one that worked for the version of emacs I was using at the
;; time I created this.  Due to this, and due to my familarity with
;; how to do what I needed in Jscheme, this code actually depends on
;; the following items being already installed: java and wget.  Java
;; is used to run the JScheme process which is used to perform
;; interaction with the wiki via xmlrpc.  Wget is used during
;; initialization to pull down the xmlrpc and jscheme jar files.
;;
;;   jscheme-7.2.jar
;;       will be downloaded from http://jscheme.sourceforge.net/jscheme/downloads/jscheme-7.2.jar
;;   xmlrpc
;;       will be downloaded from http://apache.edgescape.com/ws/xmlrpc/
;; 
;; You _must_ put the confluence.scm file into
;; $HOME/.confluence/confluence.scm, and (you probably want to) load
;; the confluence.el file in your .emacs file.
;;
;; You will also need to add:
;;
;;   (setq *confluence-xml-rpc-url* "https://your confluence instance.com/confluence/rpc/xmlrpc")
;;   (cnf-library-init)
;;
;; To your .emacs file to initialize the confluence library.
;;
;; Currently there are 2 main functions, one that gets a page and one
;; that pushes the page content back.  They are bound to "\C-xwo" and
;; "\C-xws" respectively.
;;
;;
;; The main body of the confluence xmlrpc interaction is in the
;; confluence.scm file.


;; todo: confluence-diff-buffer-vs-page

(defvar *confluence-xml-rpc-url* nil)


(defvar *cnf-rc-directory*       (format "%s/.confluence"     (getenv "HOME")))
(defvar *cnf-extlib-directory*   (format "%s/extlib"          *cnf-rc-directory*))
(defvar *cnf-xmlrpc-tgz*         (format "%s/xmlrpc.tar.gz"   *cnf-extlib-directory*))
(defvar *cnf-jscheme-jar*        (format "%s/jscheme-7.2.jar" *cnf-extlib-directory*))
(defvar *cnf-scheme-lib*         (format "%s/confluence.scm"  *cnf-rc-directory*))
(defvar *confluence-username*)
(defvar *cnf-jscheme-buffer*)

(defun cnf-wget (url destfile)
  "Pull down a url using wget if and only if the destination file does not exist."
  (if (not (file-exists-p destfile))
      (shell-command
       (format "wget -O %s %s"
               destfile
               url))))

(defun confluence-run-jscheme ()
  "Run an inferior Scheme process, input and output via buffer
*confluence-jscheme*.
If there is a process already running in `*confluence-jscheme*', switch to that
buffer."
  (if (not (comint-check-proc "*confluence-jscheme*"))
      (progn
        (set-buffer (apply 'make-comint "confluence-jscheme" "java"
                           nil (list "-jar" *cnf-jscheme-jar*)))
        (setq *cnf-jscheme-buffer* "*confluence-jscheme*")
        (comint-send-string
         (confluence-jscheme-proc)
         (format "(define *confluence-xml-rpc-url* \"%s\")"
                 *confluence-xml-rpc-url*))
        (comint-send-string
         (confluence-jscheme-proc)
         (format "(define *confluence-username* \"%s\")"
                 *confluence-username*))
        (confluence-read-and-set-password)
        (comint-send-string 
         (confluence-jscheme-proc)
         (format "(load \"%s\")\n"
                 *cnf-scheme-lib*)))))

(defun confluence-read-and-set-password ()
  (interactive)
  (comint-send-string
   (confluence-jscheme-proc)
   (format "(begin (define *confluence-password* \"%s\") #t)"
           (read-passwd "Confluence Password: "))))


(defun confluence-jscheme-proc ()
  "Return the current jscheme process.  See variable `*cnf-scheme-buffer*'."
  (let ((proc (get-buffer-process *cnf-jscheme-buffer*)))
    (or proc
        (error "No current process.  See variable `*cnf-jscheme-buffer*'"))))

(defun cnf-to-jscheme (&rest args)
  (comint-send-string 
   (confluence-jscheme-proc) 
   (apply #'format args)))

(defun confluence-file-mtime (fname)
  (cond ((file-exists-p fname)
         (let* ((fname fname)
                (attrs (file-attributes fname))
                (mtime       (nth  5 attrs )))
           (decode-time mtime)))
        (t
         nil)))

(defun confluence-wait-for-condition (function args &optional max-wait)
  (if (null max-wait)
      (setf max-wait 5000))
  (let ((waited-for 0))
    (while (and (not (apply function args))
                (< waited-for max-wait))
      (message "waiting...%s" waited-for)
      (sleep-for 0 250)
      (setf waited-for (+ waited-for 250)))
    (cond ((apply function args)
           (message "condition passed")
           t)
          (t
           (message "timeout occurred")
           nil))))


(defun confluence-wait-for-file-change (fname &optional max-wait)
  (if (null max-wait)
      (setf max-wait 5000))
  (confluence-wait-for-condition
   (lambda (orig)
     (not (equal orig
                 (confluence-file-mtime fname))))
   (list (confluence-file-mtime fname))
   max-wait))

(defun confluence-edit-page (section-page)
  "Pull down the wiki markup for PAGE-NAME in the SECTION-NAME.  Note:
this function will actually write out 2 files,
$HOME/.confluence/wiki/../SECTION-NAME/PAGE-NAME.txt and
$HOME/.confluence/wiki/../SECTION-NAME/PAGE-NAME.page The .txt file
will be the one associated to the buffer you can use to edit the page.
Once in a wiki buffer, the `confluence-store-page' function can be
used to push the page back into the wiki.  The .page file is a
serialized Java data structure with extended information about the
wiki artifact."
  (interactive "sSection/Page: ")
  (let ((section-name (car (split-string section-page "/")))
        (page-name (cadr (split-string section-page "/"))))
    (comint-send-string 
     (confluence-jscheme-proc) 
     (concat
      (format "(fetch-page {%s} {%s})\n"
              section-name
              page-name)))
    (let ((file (concat (expand-file-name "~/.confluence/wiki/need-instance-name-here/")
                        section-name "/" page-name ".txt")))
      (cond ((confluence-wait-for-file-change file)
             (message "%s loaded." section-page)
             (find-file file)
             (auto-fill-mode -1)
             (revert-buffer t t t)
             t)
            (t
             (message "Error: unable to load %s." section-page)
             nil)))))

(defun confluence-store-page ()
  "Stores the page associated with the current buffer.  Don't call
this in a non-confluence page buffer, the behavior is undefined."
  (interactive)
  (save-buffer)
  (comint-send-string 
   (confluence-jscheme-proc) 
   (format "(confluence-store-page-file {%s})\n"
           (buffer-file-name)))
  (message "Sent off for storage back to the wiki")
  (cond ((confluence-wait-for-file-change (buffer-file-name))
         (message "page stored to confluence." )
         (revert-buffer t t t)
         t)
        (t
         (message "Error storing page to confluence")
         nil)))

(defun cnf-diff-local-page-vs-wiki ()
  "Diffs the page assocaitd to the currently open buffer vs the wiki."
  (interactive)
  (if (file-exists-p "/tmp/cnf-diff.page")
      (delete-file "/tmp/cnf-diff.page"))
  (if (file-exists-p "/tmp/cnf-diff.txt")
      (delete-file "/tmp/cnf-diff.txt"))
  (cnf-to-jscheme "(fetch-page (confluence-txt-file.space {%s}) (confluence-txt-file.title {%s}) {%s} {%s})\n"
                  (buffer-file-name)
                  (buffer-file-name)
                  "/tmp/cnf-diff.page"
                  "/tmp/cnf-diff.txt")
  (cond ((confluence-wait-for-file-change "/tmp/cnf-diff.txt")
         (message "File arrived, calling diff...")
         (call-process
          "diff"
          nil
          "*cnf-diff*"
          t
          "-U2"
          (buffer-file-name)
          "/tmp/cnf-diff.txt")
         (switch-to-buffer "*cnf-diff*")
         t)
        (t
         (message "Error: unable to retreive section for diff?"))))

(defun cnf-library-init (username)
  "Initalize the confluence library and sub-process."
  (interactive "sConfluence Username: ")
  (setq *confluence-username* username)
  (make-directory *cnf-rc-directory* t)
  (make-directory *cnf-extlib-directory* t)
  (cnf-wget"http://apache.edgescape.com/ws/xmlrpc/xmlrpc-current-bin.tar.gz" *cnf-xmlrpc-tgz*)
  (shell-command
   (format "cd %s; tar xzf xmlrpc.tar.gz"
           *cnf-extlib-directory*))
  (cnf-wget
   "http://jscheme.sourceforge.net/jscheme/downloads/jscheme-7.2.jar"
   *cnf-jscheme-jar*)
  (confluence-run-jscheme))


(global-set-key "\C-xwo" 'confluence-edit-page)
(global-set-key "\C-xws" 'confluence-store-page)
(global-set-key "\C-xw=" 'cnf-diff-local-page-vs-wiki)


; (file-attributes "/home/mortis/.emacs")
;; (nil              file type
;;  1                num names the file has (hard links)
;;  1000             file's UID (user-id)
;;  1000             files' GID (group-id)
;;  (18141 23720)    atime
;;  (18047 35464)    mtime
;;  (18047 36259)    last status change time
;;  20770            file size in bytes
;;  "-rw-------"     file's permissions/modes
;;  nil              t if the file's GID would change if it were recreated
;;  834515           inode number
;;  2051)            file system number where file is stored