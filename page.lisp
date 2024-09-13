(in-package #:site)

(navi:define-tag page (body attrs)
  `(:html
     (:head
       (:link :rel "stylesheet" :type "text/css" :href "style.css")
       (:link :rel "stylesheet" :type "text/css" :href "prism.css")
       (:script :type "text/javascript" :src "socket.js")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1"))
     (:body
       (:div :id "root-container"
         (site-header
           (link :label "[home]" :url "index.html")
           (link :label "[posts]" :url "posts.html")
           (link :label "[projects]" :url "projects.html")
           (link :label "[about]" :url "about.html"))
         (:div :id "root-content" ,@attrs ,@body)))))

;; * Site header

(navi:define-tag site-header (links attrs)
  `(:div
     (:div :id "site-header" ,@attrs
       (:a :href "index.html"
         (embed-asset "logo.svg"))
       (:div :id "site-header-menu"
         (:ul :class "h-menu"
           ,@(loop for link in links collect `(:li ,link)))
         (:a :class "menu-item-icon" :href "/"
           (embed-asset "icon-rss.svg"))
         (:a :class "menu-item-icon" :href "https://github.com/chip2n"
           (embed-asset "icon-github.svg"))))
     (:hr)))

(navi/style:define-style site-header-style
  ("#site-header"
   :display "flex"
   :flex-direction "row"
   :padding "128px 0 0 0"
   :font-weight "bold")

  ("#logo"
   :height "20px"
   :margin "0 8px 0 0")

  ("#site-header-menu"
   :display "flex"
   :flex-grow 1
   :justify-content "flex-end"
   :align-items "center")

  (:media "(max-width: 600px)"
          ("#site-header"
           :padding "64px 0 0 0"
           :flex-direction "column")
          ("#site-header-menu"
           :margin-top "8px"
           :justify-content "center")
          ("#logo"
           :height "28px"))

  (.menu-item-icon
   :height "16px"
   :width "16px"
   :margin "0 0 0 12px"

   (img
    :height "16px"
    :width "16px"
    :transition "all .2s ease-in-out")

   ((:and img :hover)
    :transform "scale(1.3)"))

  (.menu-item-selected
   :font-weight "bold"
   :user-select "none")

  (ul.h-menu
   :margin "auto 0"
   :padding-inline-start "0px"

   (li
    :display "inline")))

;; * Page header

(navi:define-tag page-header (body attrs &key title src)
  `(:div :class "page-header" ,@attrs
     (:h1 ,title)
     ,(when src
        `(link :label "[source]" :url ,src))
     ,@body))

(navi/style:define-style page-style
  (.page-header
   :display "flex"
   :justify-content "space-between"
   :align-items "flex-end"
   :border-bottom "1px solid #FFFFFF20"
   :padding-bottom "8px"
   :margin-bottom "16px")

  (h1
   :color "#FFFFFF"
   :font-size "1.8rem"
   :margin "0")
  (h2
   :font-size "1.2rem"
   :color "#FFFFFF"
   :position "relative"
   :margin-top "32px")
  ("h2::before"
   :content "#"
   :position "absolute"
   :left "-24px"
   :color "#52C6EB"
   :font-weight "normal"))

;; * Global styling

(navi/style:define-style root-style
  (:import "https://rsms.me/inter/inter.css")

  (html
   ;; :font-family "'Inter', sans-serif"
   :font-family "monospace"
   :font-size "14px")

  (body
   :margin "0 auto"
   :max-width "1000px"
   :height "100%"
   :background-color "#21242B"
   :color "#FFFFFF"
   :overflow-y "scroll")

  ("#root-container"
   :margin "0px 32px")

  ("#root-content"
   :margin "32px 0px"))

(navi/style:define-style divider-style
  (.divider
   :flex-grow 1
   :border "dashed 1px #FFFFFF"
   :margin "5px")

  (hr
   :border "solid 1px #FFFFFF"))

(navi/style:define-style link-style
  (a
   :color "#52C6EB"
   :font-weight "bold"
   :font-family "monospace"
   :text-decoration "none")

  ((:and a :hover)
   :text-decoration "underline"
   :text-decoration-thickness "2px"
   :text-decoration-skip-ink "none"))
