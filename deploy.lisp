(require :asdf)
(require :website)
(setf navi:*hot-reload-p* nil)
(site:build "./public/")
