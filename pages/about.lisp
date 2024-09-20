(in-package #:site)

(navi:define-page about (:path "about.html")
  (page
    (page-header :title "About")
    (:p
      "I'm a software engineer living in Gothenburg, Sweden. I've been in the business for roughly 20 years, focusing mainly on mobile app development (both native using Kotlin and Swift, and cross-platform using the Flutter framework). I'm currently the CTO at "
      (:a :href "https://www.remente.com/" "Remente")
      ", where I'm in charge of both the Android/iOS app as well as the product design and backend. In my free time, I love messing around with both bleeding edge programming languages as well as old, crufty ones.")
    (:p
      "When I'm not glued to the computer screen, I like to explore music (and musical improvisation in particular). I play guitar, bass, saxophone as well as harmonica.")
    (:p
      "Want to get in touch? "
      (:a :href "mailto:website@arvidsson.io" "Send me an email")
      " and I'll get back to you!"
      )))
