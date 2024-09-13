(in-package #:site)

(navi:define-page posts (:path "posts.html")
  (page
    (page-header :title "Posts")
    (post-summary-list)))
