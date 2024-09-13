(in-package #:site)

(navi:define-tag link (body attrs &key label url)
  `(:a :href ,url ,@attrs ,label ,@body))

(defun out-path ()
  "/tmp/out/")

(defun asset-path (file)
  (asdf:system-relative-pathname 'website (merge-pathnames "assets/" file)))

(defun org-path (file)
  (asdf:system-relative-pathname 'website (merge-pathnames "org/" file)))

(defun tools-path (file)
  (asdf:system-relative-pathname 'website (merge-pathnames "tools/" file)))

(defun embed-asset (file)
  (spinneret:with-html
    (:raw
     (uiop:read-file-string (asset-path file)))))
