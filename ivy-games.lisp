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

(defpsmacro new-sprite (&key (x 0) (y 0) (scale 1) (image "/textures/dirt.png") (anchor "bottom_center") (flipped false))
  `(new (jaws.-sprite (create image ,image
                              scale ,scale
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
(defpsmacro _ (func (&rest body))
  `((@ _ ,func) ,@body))
(defpsmacro add-powerup (&key (x 0) (frame 1))
  (let ((nom (gensym)))
    `(let ((,nom (new-sprite :x ,x :y (- world.height (* 2 texture-size)))))
       ((@ ,nom set-image) (aref pickups-sheet.frames ,frame))
       (powerups.push ,nom)
       (blocks.push ,nom))))


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
        ,@body)))))

(defmacro with-game (&rest body)
  `(with-page
       (:div :class "span12"
             (:canvas :id "canvas" :width "800" :height "400")
             (:br)
             (:div :id "fps")
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

(defun platform2 ()
  (with-game
      (:script
       (str
        (ps
          (defvar player)

          (defvar scenery)
          (defvar bullets)
          (defvar blocks)

          (defvar fps)
          (defvar viewport)
          (defvar tile_map)
          (defvar world)
          (defvar show_stats)
          (defvar texture-size 64)

          (defvar powerups)

          (defvar roo)
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
                                    (blocks.push (new-sprite :x rx :y ry))))
                                null)
                 add-roo (lambda ()
                           (blocks.push (new-sprite :image "roo.png" :x 512 :y (- world.height (* 2 texture-size)))))
                             
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

                 add-scenery (lambda ()
                               (scenery.push (new-sprite :anchor "bottom_left"
                                                         :x 512
                                                         :scale .5
                                                         :y (- world.height (* 2 texture-size))
                                                         :image "/textures/dog_house.png")))

                 setup (lambda ()
                         (setf live_info (gebi "fps"))
                                     
                         (setf blocks (new (jaws.-sprite-list)))
                         (setf bullets (new (jaws.-sprite-list)))
                         (setf scenery (new (jaws.-sprite-list)))

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
                                                                 image "daisy.png"
                                                                 anchor "center_bottom"))))
                         (setf player.can_fire "true")

                                        ; draw the ground
                         (do ((i 0 (+ i texture-size)))
                             ((> i world.width))
                           (blocks.push (new-sprite :x i :y (- world.height texture-size))))
                             
                                        ; draw roo
                         ;; (blocks.push (new-sprite :x 256 :y (- world.height (* 2 texture-size))
                         ;;                          :image "roo.png"))

                         (platform.add-scenery)
                         (platform.add-roo)
                         (tile_map.push blocks)
                             
                         (setf player.vx 0)
                         (setf player.vy 0)
                         (setf player.can_jump true)

                         (setf player.y 64)
                         (setf jaws.context.moz-image-smoothing-enabled true)
                         (setf jaws.prevent-default-keys (array "up" "down" "left" "right" "space"))
                         null)
                 
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
                             
                          (when (jaws.pressed "space")
                            (when (eq player.can_fire "true")
                              (setf player.can_fire "false")
                              (bullets.push (new-sprite :x (@ (player.rect) right)
                                                        :y (- player.y 40)
                                                        :anchor "center"
                                                        :image "bullet.png"))
                              (set-timeout (lambda ()
                                             (setf player.can_fire "true")) 500)))

                              
                          ;; movement
                          (+= player.vy 0.4)
                          (platform.player-move)
                          (_ map (bullets.sprites (lambda (bullet)
                                                    (+= bullet.x 10))))


                          (viewport.center-around player)
                          (and show_stats
                               (setf live_info.inner-h-t-m-l
                                     (concatenate 'string jaws.game_loop.fps
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
                                                  (parse-int world.height))))
                          null)
                     
                 draw (lambda ()
                        (jaws.clear)
                        (viewport.apply (lambda ()
                                          (blocks.draw)
                                          (scenery.draw)
                                          (player.draw)
                                          (bullets.draw)
                                          null)))))

          (with-document-ready (lambda ()
                                 (jaws.assets.add (array "droid_big.png"
                                                         "the-hero-small.png"
                                                         "/blocks/blocks1.png"
                                                         "/blocks/pickups.png"
                                                         "daisy.png"
                                                         "/textures/dog_house.png"
                                                         "cherries.png"
                                                         "roo.png"

                                                         "/textures/dirt.png"
                                                         "/textures/bullet_1.png"
                                                         "/textures/fence-bg.png"
                                                         "/textures/trash_can.png"
                                                         "/textures/coop_1.png"
                                                         "/textures/flowerpot_1.png"
                                                         "/textures/tree_1.png"
                                                         "/textures/daisy_master_coffee.png"
                                                         "/textures/flowerpot_2.png"
                                                         "/textures/wing_pot_1.png"
                                                         "/textures/grass_fg.png"
                                                         "/textures/dog_house_broken.png"
                                                         "/textures/sky_2.png"

                                                         "bullet.png"
                                                         "dog-anim.png"))

                                 (jaws.start platform))))))))

