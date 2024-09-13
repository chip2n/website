(in-package #:site)

(navi:define-page index (:path "index.html")
  (page
    (page-header :title "Home")
    (:p "Welcome to my tiny corner on the information superhighway known as the World Wide Web! This is the home for my various projects and notes - not many right now, but hopefully the list will grow with time.")
    (:h2 "Recent posts")
    (post-summary-list)))

(navi:define-tag post-summary-list (body attrs)
  `(:ol :class "post-summary-list" ,@attrs
     (loop for post in (post-list)
           do (post-summary-item :post post))
     ,@body))

;; TODO reuse in post list?
(navi:define-tag post-summary-item (body attrs &key post)
  `(:li ,@attrs
     (:div (post-date ,post))
     (:a :href (post-file-name ,post)
       (post-header ,post))
     ,@body))

(navi/style:define-style index-style
  (.post-summary-list
   :padding-left "0px"

   (li
    :display "flex"
    :margin-bottom "8px"

    ((> div)
     :margin-right "16px"))))
