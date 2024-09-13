(in-package #:site)

(setf navi:*output-dir* (out-path))
(navi:add-asset (asset-path "favicon.ico"))
(navi:add-asset (asset-path "icon-github.svg"))
(navi:add-asset (asset-path "icon-rss.svg"))
(navi:add-asset (asset-path "prism.css"))

;; * Posts

(defvar *posts* (make-hash-table :test 'equal))

(defclass post ()
  ((id :initarg :id :accessor post-id)
   (export-file-name :initarg :export-file-name :accessor post-file-name)
   (export-date :initarg :export-date :accessor post-date)
   (node :initarg :node :accessor post-node)))

(defun parse-post-properties (node)
  (let ((id (navi/org:get-property node "NAVI_ID"))
        (export-file-name (navi/org:get-property node "EXPORT_FILE_NAME"))
        (export-date (navi/org:get-property node "EXPORT_DATE")))
    (when (and id export-file-name export-date)
      (make-instance 'post
        :id id
        :export-file-name export-file-name
        :export-date export-date
        :node node))))

(defun post-list ()
  "Get list of posts ordered by date."
  (let ((posts (alexandria:hash-table-values *posts*)))
    (sort posts #'string> :key #'post-date)))

(defun post-header (post)
  (parse-org-headline
   (navi/org:section-headline (post-node post))))

(defun post-tags (post)
  (nth-value 1 (parse-org-headline
                (navi/org:section-headline (post-node post)))))

(defun parse-org-headline (headline)
  "Returns trimmed headline and a list of tags."
  (let ((i (1- (length headline)))
        (result nil))
    (if (char= (schar headline i) #\:)
        (progn
          (loop for j from (1- (length headline)) downto 0
                for c = (schar headline j)
                until (char= c #\ )
                do (push c result)
                finally (setf i j))
          (values
           (str:trim (str:substring 0 i headline))
           (str:split-omit-nulls #\: (format nil "~{~A~}" result))))
        (str:trim headline))))

(navi:define-tag blog-page (body attrs &key post)
  `(page ,@attrs
     (page-header :title (post-header ,post))
     (:div :class "post-meta-bar"
       (:span "2024-01-01")
       (:ul :class "h-menu"
         (loop for tag in (post-tags ,post)
               do (:li :class "post-meta-bar-tag" (str:concat "#" tag)))))
     (loop for node in (navi/org:nodes (post-node ,post))
           do (:raw (spinneret:with-html-string (render-node node))))
     ,@body))

(navi/style:define-style blog-style
  "Style for blog pages."
  (.post-meta-bar
   :display "flex"
   :align-items "center"
   :margin-bottom "32px"
   (.h-menu
    :margin-left "16px")
   (.post-meta-bar-tag
    :color "#ffffff80")))

;; TODO
(defmethod render-node (node)
  (warn "Cannot render node ~A~%" node))

;; TODO this seems to add a whitespace after the link...
(defmethod render-node ((node navi/org:link-node))
  (spinneret:with-html
    (:a :href (navi/org:link-url node)
      (navi/org:link-description node))))

(defmethod render-node ((node navi/org:source-node))
  (let ((highlight-code
          (with-output-to-string (s)
            (uiop:with-temporary-file (:stream temp :pathname path :keep t)
              (write-string (navi/org:source-code node) temp)
              (finish-output temp)
              (uiop:run-program
               (format nil "node ~a ~a"
                       (navi/utils:system-path "tools/syntax.js")
                       path)
               :output s
               :error-output *standard-output*)))))
    (spinneret:with-html
      (:div :class "org-source-block"
        (:raw (format nil "<pre><code>~A</code></pre>" highlight-code))))))

(navi/style:define-style org-mode-style
  "Style for org-mode elements."
  (.org-source-block
   :background "#1B1E24"
   :outline "1px solid #FFFFFF20"
   :border-radius "16px"
   :padding "0 16px"
   :overflow "scroll"

   (pre :margin "16px 0")
   (code :font-family "monospace")))

(defmethod render-node ((node navi/org:paragraph-node))
  (spinneret:with-html
    (:p (loop for child in (navi/org:nodes node)
              do (render-node child)))))

(defmethod render-node ((node navi/org:text-node))
  ;; TODO style
  (spinneret:with-html
    (:span
      (if (eq (navi/org:text-style node) :bold)
          (:b (navi/org:text-text node))
          (navi/org:text-text node)))))

(defmethod render-node ((node navi/org:section-node))
  (spinneret:with-html
    ;; TODO indentation level
    (:h2 (navi/org:section-headline node))
    (loop for child in (navi/org:nodes node)
          do (render-node child))))

;; TODO do lazily on first build?
;; Currently only considers top-level sections with export properties set
(progn
  (setf *posts* (make-hash-table :test 'equal))

  (let ((org (navi/org:parse-file "/home/chip/dev/personal-site/refactor/site/org/blog.org")))
    (loop for node in (navi/org:nodes org)
          for post = (parse-post-properties node)
          when post
            do (setf (gethash (post-file-name post) *posts*) post)))

  (loop for post in (post-list)
        do (let ((p post))
             (navi:add-page
              (intern (str:upcase (post-id p)))
              (post-file-name p)
              ;; TODO Don't want to have to duplicate the page body like this
              `((blog-page :post ,p))
              (lambda () (spinneret:with-html (:doctype)
                      (blog-page :post p))))))

  (navi:build-pages))

(defun start ()
  (navi:start))

(defun stop ()
  (navi:stop))
