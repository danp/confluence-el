;; -*- Scheme -*-
;; confluence xml-rpc interface 
;;
;;
(use-module "elf/classpath.scm")
(use-module "using/run.scm")
(addClasspathUrl {[($ "user.home")]/projects/hmsCommon/jscheme})

;; this is hard-coded and expects to have already been set up by confluence.el
(for-each addClasspathUrl
          (files** {[($ "user.home")]/.confluence/extlib/}
                  (lambda (f) (.endsWith (.getPath f) ".jar"))))

(import "org.apache.xmlrpc.client.XmlRpcClient")
(import "org.apache.xmlrpc.client.XmlRpcClientConfigImpl")
(import "java.net.URL")

;; (if (not t) (begin body))
(define-macro (unless t . body)
  `(cond ((not ,t)
          ,@body)))

(define-macro (while condition . body)
  `(do ()
       ((not ,condition) #t)
     ,@body))

;(define *confluence-xml-rpc-url* "https://intranet.hmsonline.com/confluence/rpc/xmlrpc")
;(define (set-confluence-xml-rpc-url url)
;  (set! *confluence-xml-rpc-url* url))

(define *xml-rpc-config* 
  (let ((cfg (XmlRpcClientConfigImpl.)))
    (.setServerURL cfg (URL. *confluence-xml-rpc-url*))
    cfg))

(define *xml-rpc-client* (XmlRpcClient.))
(.setConfig *xml-rpc-client* *xml-rpc-config*)

(define (new-vector class . elts)
  (let ((vec (make-vector (length elts) class)))
    (dotimes (ii (length elts))
      (vector-set! vec ii (.nth elts ii)))
    vec))

(define *confluence-rc-directory* (string-append ($ "user.home") "/.confluence/"))

(define (println . rest)
  (define (to-string aa)
    (if (eq? #null aa) "" (.toString aa)))
  (for-each (lambda (e) (.print System.out$ (to-string e))) rest)
  (.println System.out$ ""))

(define-macro (with-buffered-file-reader decl-spec . body)
  (let ((name (car decl-spec))
        (file (cadr decl-spec)))
    `(let ((result '())
           (,name
            (java.io.BufferedReader. (java.io.FileReader. ,file))))
       (Procedure.tryFinally
        (lambda () ; try 
         (set! result (begin ,@body)))
        (lambda () ; finally
         (.close fr)))
       result)))

(define-method (drain-reader->string (input-stream java.io.Reader))
  (let ((buff (make-array char.class 1024))
        (sb (StringBuffer.))
        (result -1))
    (set! result (.read input-stream buff))
    (while
     (not (= -1 result))
     (.append sb buff 0 result)
     (set! result (.read input-stream buff)))
    (.toString sb)))


;; read the file as a single string
(define (read-file file)
  (with-buffered-file-reader
   (fr file)
   (drain-reader->string fr)))

(define (write-file file items)
  (let ((fw (java.io.FileWriter. (java.io.File. file))))
    (cond ((list? items)
           (for-each (lambda (e)
                       (.write fw e))
                     items))
          (else
           (.write fw items)))
    (.close fw)))

(define (object->file file-name thing)
  (let ((objstr
         (java.io.ObjectOutputStream. (java.io.FileOutputStream. file-name))))
    (.writeObject objstr thing)
    (.close objstr)))

(define (object<-file file-name)
  (let* ((objis
          (java.io.ObjectInputStream. (java.io.FileInputStream. file-name)))
         (thing (.readObject objis)))
    (.close objis)
    thing))

(define (config-file file)
  (string-append *confluence-rc-directory* file))

(define (page-file section-name page-name)
  (string-append *confluence-rc-directory* "/wiki/need-instance-name-here/" section-name "/" page-name))

(.mkdirs (File. (string-append ($ "user.home") "/.confluence")))

;; fixme: also write to $HOME/.confluence at application exit time
(define *user-params* 
  (begin
    (unless (.exists (File. (config-file "params.bin")))
      (object->file (config-file "params.bin") (java.util.HashMap.)))
    (object<-file (config-file "params.bin"))))


(define (prompt-user-for-entry description default hidden)
  (let ((command `(zenity "--entry" "--text" ,description "--entry-text" ,default 
                          ,(if hidden "--hide-text" "")))
        (result ""))
    (set! result (.readLine (inputReader (run (cmd ,@command)))))
    (if (or (isNull result)
            (= 0 (.length result)))
        default
        result)))

(define (get-user-parameter param-name description default-value hidden)
  (let ((val (if (.containsKey *user-params* param-name)
                 (.get *user-params* param-name)
                 #f)))
    (unless val
      (set! val (prompt-user-for-entry description default-value hidden)))
    (unless hidden
      (.put *user-params* param-name val)
      (object->file (config-file "params.bin") *user-params*))
    val))

(define (confluence-log-in)
  (.execute *xml-rpc-client* "confluence1.login" (new-vector String.class *confluence-username* *confluence-password*)))

(define *confluence-session-token* (confluence-log-in))
  


; org.apache.xmlrpc.XmlRpcException: java.lang.Exception: com.atlassian.confluence.rpc.InvalidSessionException: User not authenticated or session expired. Call login() to open a new session
(define (make-confluence-method method-name)
  (let ((executor
         (lambda R 
           (.execute *xml-rpc-client* 
                     {confluence1.[method-name]} 
                     (list->vector (cons *confluence-session-token* R))))))
  (lambda R
    (tryCatch
     (apply executor R)
     (lambda (exception)
       (set! *confluence-session-token* (confluence-log-in))
       (apply executor R))))))
       

;(.execute *xml-rpc-client* "confluence1.getServerInfo" (new-vector Object.class *confluence-session-token*))

(define *help* '())
(define-macro (define-confluence-method name)
  (println {**method: [name]})
  (set! *help* (cons 
                {[name] => c:[name]} 
                *help*))
  `(begin
     (define ,(string->symbol {c:[name]})
       (make-confluence-method ,name))
     (define ,(string->symbol {pc:[name]})
       (lambda R (pp (apply ,(string->symbol {c:[name]}) R))))))

;; see: http://confluence.atlassian.com/display/DOC/Remote+API+Specification
;; for documentation on all the api methods
(for-each
 (lambda (name)
   (eval `(define-confluence-method ,name)))
 (map .toString
      '(getServerInfo
        getSpaces
        getSpace
        getPages
        getPage
        getPageHistory
        getPagePermissions
        getAttachements
        getAncestors
        getChildren
        getDescendents
        getComment

        addComment
        removeComment
        storePage
        removePage
        getAttachmentData
        moveAttachment
        getBlogEntries
        storeBlogEntry
        getBlogEntryByDayAndTitle
        search)))


(define gensym
  (let ((ctr 0))
    (lambda PFX
      (set! ctr (+ 1 ctr))
      (cond ((null? PFX)
             (string->symbol (string-append "gsym-" ctr)))
            (else
             (string->symbol (string-append "gsym-" (first PFX) "-" ctr)))))))

(define-macro (map-bind forms the-map . body)
  `(let (,@(map (lambda (elt)
                  (list elt `(.get ,the-map ,(.toString elt))))
                forms))
     ,@body))

;; (map-bind 
;;   (title type excerpt id) 
;;   (vector-ref (c:search "Matching Service" 10) 0) 
;;   (println {title: [title] => [excerpt]}))

;(c:getPages "SWDEV")

(define (page-content page)        (.get page "content"))
(define (page-title page)          (.get page "title"))
(define (page-space page)          (.get page "space"))
(define (page-id page)             (.get page "id"))
(define (page-parent-id page)      (.get page "parentId"))
(define (page-created page)        (.get page "created"))
(define (page-parent-id page)      (.get page "parentId"))
(define (page-current page)        (.get page "current"))
(define (page-modifier page)       (.get page "modifier"))
(define (page-persion page)        (.get page "persion"))
(define (page-modified page)       (.get page "modified"))
(define (page-creator page)        (.get page "creator"))
(define (page-home-page page)      (.get page "homePage"))
(define (page-permissions page)    (.get page "permissions"))
(define (page-contentStatus page)  (.get page "contentStatus"))
(define (page-url page)            (.get page "url"))

;; fixme: use a coupld of variables to hold the last thing that people did...
;(define *confluence-last-section* "SWDEV")
;(define *confluence-last-page* "MatchingService")

(define (fetch-page section-name page-name . args)
  (let* ((page (c:getPage section-name page-name))
        (file-name (if (null? args) (page-file section-name page-name)
                       (first args)))
        (textfile (if (null? args) {[file-name].txt} (second args))))
    (.mkdirs (.getParentFile (File. file-name)))
    (println {fetching: [section-name] / [page-name]})
    (object->file file-name page)
    (write-file textfile (page-content page))))

(define (map-put m k v)
  (let ((n (java.util.HashMap. m)))
    (.put n k v)
    n))


(define (store-page section-name page-name)
  (let* ((file-name (page-file section-name page-name))
         (page (object<-file {[file-name]}))
         (content (read-file {[file-name].txt})))
    (println {size of file: [(.length content)]})
    (println {size of old content: [(.length (.get page "content"))]})
    (c:storePage (map-put page "content" content))
    (fetch-page section-name page-name)))

(define (confluence-store-page-file file)
  (let ((page (object<-file (.replaceFirst file ".txt$" ""))))
    (store-page (page-space page) (page-title page))))

(define (confluence-txt-file->page-file file)
  (.replaceFirst file ".txt$" ""))

(define (confluence-txt-file->page file)
  (object<-file (.replaceFirst file ".txt$" "")))

(define (confluence-txt-file.space txtfile)
  (.get (confluence-txt-file->page txtfile) "space"))

(define (confluence-txt-file.title txtfile)
  (.get (confluence-txt-file->page txtfile) "title"))

