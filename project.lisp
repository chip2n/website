(in-package #:site)

(navi:define-tag project-root (body attrs &key title src hero sidebar)
  `(:div ,@attrs
     ,hero

     (page-header :title ,title :src ,src)

     (:div :style "display: flex;"
       (:div :style "flex-grow: 1; margin-right: 16px" ,@body)
       (project-sidebar ,sidebar))))

(navi:define-tag project-sidebar (body attrs)
  `(:div :class "game-feature-box" ,@attrs
     ,@body))

;; * Styles

(navi/style:define-style game-container
  ("#game-container"
   :display "flex"
   :background-color "#000000"
   :outline "1px solid #FFFFFF20"
   :border-radius "16px"
   :margin-bottom "32px"
   :overflow "hidden"
   :width "100%"
   :justify-content "center"))

(navi/style:define-style game-canvas-style
  (.game
   :width "640px"
   :height "480px"
   :border "0"
   :overflow "hidden"
   :display "block"
   :image-rendering "optimizeSpeed"
   :image-rendering "-moz-crisp-edges"
   :image-rendering "-o-crisp-edges"
   :image-rendering "-webkit-optimize-contrast"
   :image-rendering "optimize-contrast"
   :image-rendering "crisp-edges"
   :image-rendering "pixelated"
   :-ms-interpolation-mode "nearest-neighbor")
  (:media "(max-width: 639px)"
          (.game
           :width "320px"
           :height "240px")))

(navi/style:define-style game-feature-box
  (.game-feature-box
   :background-color "#1F2228"
   :border "1px solid #FFFFFF20"
   :border-radius "16px"
   :padding "0px 48px 0px 20px"
   
   (ul
    :padding-inline-start "24px")

   ((> ul)
    :padding 0

<    ((> li)
     :list-style-type "none"
     :margin 0
     :padding 0))))
