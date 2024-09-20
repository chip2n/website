(in-package #:site)

(navi:add-asset (asset-path "zball.png"))
(navi:add-asset (asset-path "navi.svg"))

(navi:define-page project-list (:path "projects.html")
  (page
    (page-header :title "Projects")
    (:div :class "project-list"
      (project-list-item
        :title "ZBall"
        :description "A clone of the classic Breakout/Arkanoid game, with way too many particle effects added."
        :img (:img :class "project-thumb" :src "zball.png")
        :url "project-zball.html")
      (project-list-item
        :title "Navi"
        :description "A static site generator written in Common Lisp, used to generate the code for this site."
        :img (:img :class "project-thumb" :src "navi.svg")
        :url "project-navi.html"))))

(deftag project-list-item (body attrs &key title description img url)
  `(:div :class "project-list-item" ,@attrs
    ,img
    (:div :class "project-list-content"
      (:h1 ,title)
      (:span ,description)
      ;; TODO refactor
      (:ul :class "h-menu"
        (:li (link :label "[read more]" :url ,url))
        (:li (link :label "[source]" :url ,url))
        )
      ,@body)))

(navi/style:define-style project-list-style
  (.project-list
   :display "flex"
   :flex-direction "column"
   :gap "16px"
   )

  (.project-list-item
   :display "flex"
   :gap "8px"

   (h1
    :font-size "1.2em"))

  (.project-list-content
   :display "flex"
   :flex-direction "column"
   :justify-content "center"
   :margin "8px")

  (.project-thumb
   :width "128px"
   :height "96px"
   :flex-shrink "0"
   :flex-grow "0"
   :border-radius "16px"
   :outline "1px solid #FFFFFF20"
   :object-fit "cover"))
