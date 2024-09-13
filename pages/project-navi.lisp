(in-package #:site)

(navi:define-page project-navi (:path "project-navi.html")
  (page
    (project-root
      :title "Navi"
      :src "https://github.com/chip2n/navi"

      :sidebar
      (:ul
        (:li (:span "Language: " (link :label "Common Lisp" :url "https://lisp-lang.org/")))
        (:li "Platforms:"
          (:ul
            (:li "Linux")))
        (:li "Dependencies:"
          (:ul
            (:li (link :label "spinneret" :url "https://github.com/ruricolist/spinneret"))
            (:li (link :label "lass" :url "https://github.com/Shinmera/LASS")))))
      (:div
        (:span
          "A static site generator written in Common Lisp, used to generate the code for this site. It supports generating pages entirely through code, and ships with functions to process "
          (:a :href "https://orgmode.org/" "org-mode")
          " files. It also supports automatic page reloading when any page or tag is redefined.")
        (:h2 "Rationale")
        (:span
          "There's many static site generators available - why make a new one? The main reason is, of course, because it's fun to write one! The distant second reason is that I wanted to have full control over the outputted HTML. I knew that I would need to run arbitrary code during the build process, so any generator that didn't allow for this (or makes you jump through hoops to do so) was out the window. I also knew that I wanted to write the HTML using "
          (:a :href "https://en.wikipedia.org/wiki/S-expression" "S-expressions")
          ". I find them strictly superior to the tag-soup that is regular HTML. They are easy to generate through code and are very quick to modify using structural editing. What's left is mostly toy projects, so why not add another toy project to the pile?")
        (:h2 "Implementation")
        (:span
          "Navi is implemented in Common Lisp. It uses Spinneret to render the S-expressions to HTML, and ")
        (:a :href "https://shinmera.com/" "Shinmera")
        (:span "'s excellent LASS library for outputting CSS.")
        (:span
          "Nav Common Lisp. It uses Spinneret to render the S-expressions to HTML, and "
          (:a :href "https://shinmera.com/" "Shinmera")
          "'s excellent LASS library for outputting CSS.")))))
