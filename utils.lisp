(in-package #:site)

(navi:define-tag link (body attrs &key label url)
  `(:a :href ,url ,@attrs ,label ,@body))

;; TODO actually want a separate system for the site and the static site generator
(defun out-path ()
  (asdf:system-relative-pathname 'personal-site "refactor/out/"))

(defun asset-path (file)
  (asdf:system-relative-pathname 'personal-site (merge-pathnames "refactor/site/assets/" file)))

(defun org-path (file)
  (asdf:system-relative-pathname 'personal-site (merge-pathnames "refactor/site/org/" file)))

(defun embed-asset (file)
  (spinneret:with-html
    (:raw
     (uiop:read-file-string (asset-path file)))))
