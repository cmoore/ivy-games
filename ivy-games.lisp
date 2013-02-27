;;;; ivy-games.lisp

(in-package #:ivy-games)


(defparameter *site-acceptor* nil)

(defun start-server (&key (port 8080))
  (setq *site-acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :document-root (cl-ivy:resource-path "./resources/static")
                       :port port))
  (hunchentoot:start *site-acceptor*))

(setq hunchentoot:*dispatch-table*
      (list
       (create-regex-dispatcher "^/local-css$" 'site-css)
       (create-regex-dispatcher "^/tilemap$" 'tile-map)
       (create-regex-dispatcher "^/platform$" 'platform)
       (create-regex-dispatcher "^/platform2$" 'platform2)
       (create-regex-dispatcher "^/$" 'game-index)))

(defun site-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
   (("body") (:font-family "Open Sans"))))

(defpsmacro new-sprite (&key (x 0) (y 0) (image "grass-dirt.png") (anchor "bottom_center") (flipped false))
  `(new (jaws.-sprite (create image ,image
                              x ,x
                              y ,y
                              flipped ,flipped
                              anchor ,anchor))))
(defpsmacro with-document-ready (&rest body)
  `((@ ($ document) ready) ,@body))
(defpsmacro -= (a b)
  `(setf ,a (- ,a ,b)))
(defpsmacro += (a b)
  `(setf ,a (+ ,a ,b)))
(defpsmacro gebi (name)
  `(document.get-Element-By-Id ,name))
(defpsmacro jlog (message)
  `(jaws.log ,message 1))


(defmacro with-page (&rest body)
  `(cl-who:with-html-output-to-string
       (*standard-output* nil :prologue t :indent nil)
     (htm
      (:html
       (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")

        (:link :rel "stylesheet" :href "/bs/css/bootstrap.css")
        (:link :rel "stylesheet" :href "/bs/css/bootstrap-responsive.css")
        (:link :type "text/css" :rel "stylesheet" :href "http://fonts.googleapis.com/css?family=Open+Sans:400,300,600|Merriweather:400,300,700")
        (:link :rel "stylesheet" :href "/local-css")

        ;; (:link :rel "shortcut icon" :href "/favicon.ico")
        ;; (:link :rel "apple-touch-icon" :href "/bs/img/apple-touch-icon.png")
        ;; (:link :rel "apple-touch-icon" :sizes "72x72" :href "/bs/img/apple-touch-icon-72x72.png")
        ;; (:link :rel "apple-touch-icon" :sizes "114x114" :href "/bs/img/apple-touch-icon-114x114.png")

        (:script :src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js")
        (:script :type "text/javascript" :src "/bs/js/bootstrap.min.js")
        (:script :src "/underscore.js")
        (:script :src "/js/jaws.js"))
       (:body
        (:div :class "container"
              (:div :class "row" :style "margin-top: 10px;"
                    ,@body)))))))

(defmacro with-game (&rest body)
  `(with-page
       (:div :class "span12"
             (:canvas :id "canvas" :width "1000" :height "640")
             (:br)
             (:div :id "fps")
             (:div :id "jaws-log")
             ,@body)))

(defun game-index ()
  (with-page
      (:ul
       (:li (:a :href "/tilemap" "Tile Map"))
       (:li (:a :href "/platform" "Platform"))
       (:li (:a :href "/platform2" "Platform2")))))

(defun platform ()
  (with-game
      (:script :src "/platform.js")))

(defun tile-map ()
  (with-game
      (:script :src "/tilemap.js")))


(defpsmacro _ (func (&rest body))
  `((@ _ ,func) ,@body))

(defpsmacro add-powerup (&key (x 0) (frame 1))
  (let ((nom (gensym)))
    `(let ((,nom (new-sprite :x ,x :y (- world.height (* 2 texture-size)))))
       ((@ ,nom set-image) (aref pickups-sheet.frames ,frame))
       (powerups.push ,nom)
       (blocks.push ,nom))))

(defun platform2 ()
  (with-game
      (:script
       (str (ps
              (defvar player)
              (defvar dog)
              (defvar blocks)
              (defvar fps)
              (defvar viewport)
              (defvar tile_map)
              (defvar world)
              (defvar show_stats)
              (defvar texture-size 64)

              (defvar powerups)

              (defvar roo)
              (defvar daisy)
              (defvar blocks-sheet)
              (defvar pickups-sheet)

              (setf platform
                    (create

                     drop-powerups (lambda ()
                                     (dotimes (i 20)
                                       (add-powerup :x (+ 30 (* 32 i)) :frame i)))

                     random-tiles (lambda ()
                                    (dotimes (i 100)
                                      (let ((rx (* texture-size (parse-int (* 100 (-math.random)))))
                                            (ry (- world.height (* texture-size (parse-int (* 10 (-math.random)))))))
                                        (blocks.push (new-sprite :x rx :y ry)))))
                     
                     player-move (lambda ()
                                   (+= player.x player.vx)
                                   (if (> (@ (tile_map.at-rect (player.rect)) length) 0)
                                       (-= player.x player.vx))
                                   (setf player.vx 0)
                                   
                                   (+= player.y player.vy)
                                   (defvar block (aref (tile_map.at-rect (player.rect)) 0))
                                   (if block
                                       (progn
                                         (if (> player.vy 0)
                                             (progn
                                               (setf player.can_jump true)
                                               (setf player.y (- (@ (block.rect) y) 1))))
                                         (if (< player.vy 0)
                                             (setf player.y (+ (@ (block.rect) bottom) player.height)))
                                         (setf player.vy 0))))

                     setup (lambda ()
                             (setf live_info (gebi "fps"))
                                     
                             (setf blocks (new (jaws.-sprite-list)))
                             (setf world (new (jaws.-rect 0 0 3200 640)))

                             (setf blocks-sheet (new (jaws.-sprite-sheet
                                                      (create image "/blocks/blocks1.png"
                                                              frame_size (array 32 32)
                                                              scale_image 2))))


                             (setf powerups (new -array))
                             (setf pickups-sheet (new (jaws.-sprite-sheet
                                                       (create image "/blocks/pickups.png"
                                                               frame_size (array 34 42)
                                        ;scale_image 2
                                                               orientation "right"))))


                             (setf tile_map (new (jaws.-tile-map
                                                  (create size (array 1000 1000)
                                                          cell_size (array texture-size texture-size)))))
                             
                             (setf viewport (new (jaws.-viewport (create max_x world.width
                                                                         max_y world.height))))
                             
                             (setf player (new (jaws.-sprite (create x 128
                                                                     y 128
                                                                     anchor "center_bottom"))))

                                        ; draw the ground
                             (do ((i 0 (+ i texture-size)))
                                 ((> i world.width))
                               (blocks.push (new-sprite :x i :y (- world.height texture-size))))
                             
                                        ; draw roo
                             (blocks.push (new-sprite :x 256 :y (- world.height (* 2 texture-size))
                                                      :image "roo.png"))

                             (tile_map.push blocks)
                             
                             (setf player.vx 0)
                             (setf player.vy 0)
                             (setf player.can_jump true)

                             (player.set-image "daisy.png")
                             (setf player.y 64)
                             (setf jaws.context.moz-image-smoothing-enabled true)
                             (setf jaws.prevent-default-keys (array "up" "down" "left" "right" "space")))
                     
                     update (lambda ()
                              (setf show_stats 1)
                              (when (jaws.pressed "left")
                                (unless player.flipped
                                  (player.flip))
                                (setf player.vx (- 2)))
                              
                              (when (jaws.pressed "right")
                                (when player.flipped
                                  (player.flip))
                                (setf player.vx 2))

                              (when (jaws.pressed "up")
                                (when player.can_jump
                                  (setf player.vy (- 7.5))
                                  (setf player.can_jump false)))
                                       
                              ;; movement
                              (+= player.vy 0.4)
                              (platform.player-move)

                              (viewport.center-around player)
                              (and show_stats
                                   (setf live_info.inner-h-t-m-l (concatenate 'string jaws.game_loop.fps
                                                                              "fps. P: "
                                                                              (parse-int player.x)
                                                                              "/"
                                                                              (parse-int player.y)
                                                                              "  PV "
                                                                              (parse-int player.vx)
                                                                              "/"
                                                                              (parse-int player.vy)
                                                                              "  Viewport: "
                                                                              (parse-int viewport.x)
                                                                              "/"
                                                                              (parse-int viewport.y)
                                                                              " World: "
                                                                              (parse-int world.width)
                                                                              "/"
                                                                              (parse-int world.height)))))
                     
                     draw (lambda ()
                            (jaws.clear)
                            (viewport.apply (lambda ()
                                              (blocks.draw)
                                        ;(_ map (powerups (lambda (x) ((@ (x.rect) draw)))))
                                              (player.draw)
                                        ;((@ (player.rect) draw))
                                              )))))

              (with-document-ready (lambda ()
                                     (jaws.assets.add (array "droid_big.png"
                                                             "the-hero-small.png"
                                                             "/blocks/blocks1.png"
                                                             "/blocks/pickups.png"
                                                             "daisy.png"
                                                             "cherries.png"
                                                             "roo.png"
                                                             "dog-anim.png"
                                                             "grass-dirt.png"))

                                     (jaws.start platform))))))))

