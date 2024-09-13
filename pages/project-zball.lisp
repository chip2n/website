(in-package #:site)

(navi:add-asset (asset-path "game.wasm"))
(navi:add-asset (asset-path "game.js"))

(navi:define-page project-zball (:path "project-zball.html")
  (page
    (project-root
      :title "ZBall"
      :src "https://github.com/chip2n/sokol-breakout"

      :hero
      (game-canvas)

      :sidebar
      (:ul
        (:li (:span "Language: " (link :label "Zig" :url "https://ziglang.org/")))
        (:li "Platforms:"
          (:ul
            (:li "Windows")
            (:li "Mac")
            (:li "Linux")
            (:li "Web")))
        (:li "Dependencies:"
          (:ul
            (:li (link :label "sokol" :url "https://github.com/floooh/sokol"))
            (:li (link :label "stb_image" :url "https://github.com/nothings/stb")))))

      (:div
        "A clone of the classic Breakout/Arkanoid game, with way too many particle effects added. I wrote this game mainly as an exercise in actually finishing a project for once. I picked Breakout since I figured it would be one of the simpler games to make, while still providing the opportunity to extend it with more fancy stuff through power ups."
        (:h2 "Implementation")
        "The game is implemented using the Zig programming language. Rendering is handled with the excellent sokol library (through the sokol-zig bindings), allowing it to be exported to multiple platforms including the web (through WASM)."
        (:h2 "Controls")
        (:ul
          (:li "Mouse / Arrow keys: Move the paddle")
          (:li "Space: Activate power-up"))))))

(deftag game-canvas (body attrs)
  `(:div ,@attrs
     (:div :id "game-container"
       (:canvas :class "game" :id "canvas" :oncontextmenu "event.preventDefault()"))
     (:script "
      var Module = {
        preRun: [],
        postRun: [],
        print: (function() {
            return function(text) {
                text = Array.prototype.slice.call(arguments).join(' ');
                console.log(text);
            };
        })(),
        printErr: function(text) {
            text = Array.prototype.slice.call(arguments).join(' ');
            console.error(text);
        },
        canvas: (function() {
            return document.getElementById('canvas');
        })(),
        setStatus: function(text) { },
        monitorRunDependencies: function(left) { },
      };
      window.onerror = function() {
        console.log(\"onerror: \" + event.message);
      };")
     (:script :async t :src "./game.js")
     ,@body))
