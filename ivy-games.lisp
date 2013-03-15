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
       (create-regex-dispatcher "^/flay$" 'flay)
       (create-regex-dispatcher "^/$" 'game-index)))

(defun site-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (css-lite:css
   (("body") (:font-family "Open Sans"))))

(defpsmacro new-sprite (&key (x 0) (y 0) (scale 1) (image "/img/dirt.png") (anchor "bottom_center") (flipped false))
  `(new (jaws.-sprite (create image ,image
                              scale ,scale
                              x ,x
                              y ,y
                              flipped ,flipped
                              anchor ,anchor))))

(defpsmacro jlog (message)
  `(jaws.log ,message 1))

(defpsmacro add-powerup (&key (x 0) (frame 1))
  (let ((nom (gensym)))
    `(let ((,nom (new-sprite :x ,x :y (- world.height (* 2 texture-size)))))
       ((@ ,nom set-image) (aref (@ pickups-sheet frames) ,frame))
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
             (:canvas :style "border:1px solid black;" :id "canvas" :width "800" :height "400")
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
  (with-page
      (:div :class "span12" :style "margin-top:5px;"
            (:canvas :style "border:1px solid black;" :id "canvas" :width "800" :height "400")
            (:br)
            (:div :id "fps")
            (:script
             (str
              (ps
                (defvar player)
                (defvar blocks)

                (defvar fps)
                (defvar viewport)
                (defvar tile_map)
                (defvar world)
                (defvar show_stats)
                (defvar texture-size 64)

                (defvar blocks-sheet)
                (defvar pickups-sheet)

                (setf iv
                      (create
                       fire (lambda ()
                              (when (eq player.can_fire "true")
                                (setf player.can_fire "false")
                                (let ((shot (new-sprite :x (if player.flipped
                                                               (- (@ (player.rect) right) 65)
                                                               (@ (player.rect) right))
                                                        :y (- player.y 40)
                                                        :image "/img/bullet.png")))
                                  (setf shot.vx (if player.flipped
                                                    -10
                                                    10))
                                  (setf shot.health 50 )
                                  (setf shot.vy 0)
                                  (setf shot.bullet 1)
                                  (blocks.push shot))

                                (set-timeout (lambda ()
                                               (setf player.can_fire "true")) 500)))
                       test_trash_can (lambda ()
                                        (let ((trash-can (new-sprite :x player.x
                                                                     :y player.y
                                                                     :image "/img/trash_can.png")))
                                          (setf trash-can.shootable 1)))
                       create_rat (lambda ()
                                    (let ((new-rat (new-sprite :x (- player.x 128)
                                                               :y player.y
                                                               :scale 1.5
                                                               :image "/img/rat.png")))
                                      (setf new-rat.autonymous 1)
                                      (setf new-rat.shootable 1)
                                      (setf new-rat.health 100)
                                      (blocks.push new-rat)))
                       create_chicken_black (lambda ()
                                              (let ((new-chicken (new-sprite :x (- player.x 128)
                                                                             :y player.y
                                                                             :image "/img/roo.png")))
                                                (setf new-chicken.autonymous 1)
                                                (setf new-chicken.shootable 1)
                                                (setf new-chicken.health 200)
                                                (blocks.push new-chicken)))
                       draw_text (lambda (text)
                                   (setf jaws.context.font "20pt Arial")
                                   (setf jaws.context.line-width 1)
                                   (setf jaws.context.fill-style "red")
                                   (setf jaws.context.stroke-style "rgba(200,200,200,0.0)")
                                   (jaws.context.fill-text text 10 10))
                       add_auto (lambda ()
                                  (let ((ohgod (new-sprite :x player.x
                                                           :y (- world.height 128)
                                                           :image "roo.png")))
                                    (setf ohgod.autonymous 1)
                                    (blocks.push ohgod)))

                       all_shootable (lambda ()
                                       (_ filter (blocks.sprites (lambda (x)
                                                                   (= x.shootable 1)))))

                       all_autonymous (lambda ()
                                        (_ filter (blocks.sprites (lambda (x)
                                                                    (= x.autonymous 1)))))

                       all_bullets (lambda ()
                                     (_ filter (blocks.sprites (lambda (x)
                                                                 (= x.bullet 1)))))

                       all_blocking (lambda ()
                                      (_ filter (blocks.sprites (lambda (x)
                                                                  (= x.blocking 1)))))

                       ground_level (lambda ()
                                      (- world.height 5))

                       is_outside (lambda (o)
                                    (or (< o.x 0)
                                        (> o.x (@ world width))))

                       handle_collision (lambda (bullet target)
                                          (-= target.health bullet.health)
                                          (console.log target.health)
                                          (when target.shot_image
                                            (target.set-image target.shot_image))
                                          (blocks.remove bullet))
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
                                 (let ((the-roo (new-sprite :image "/img/roo.png"
                                                            :x 512
                                                            :y (- world.height (* 2 texture-size)))))
                                   (iv.make_autonymous the-roo)
                                   (iv.make_shootable the-roo)
                                   (blocks.push the-roo)))

                       add-scenery (lambda ()
                                        ; draw the ground
                                     (do ((i 0 (+ i texture-size)))
                                         ((> i world.width))
                                       (let ((new-block (new-sprite :anchor "top_left" :x i :y (iv.ground_level))))
                                         (setf (@ new-block blocking) 1)
                                         (blocks.push new-block)))

                                     (iv.add_doghouse :x 512 :y (iv.ground_level)))


                       add_doghouse (lambda (&key (x 0) (y 0))
                                      (let ((doghouse (new-sprite :anchor "bottom_center"
                                                                  :x x
                                                                  :scale .5
                                                                  :y y
                                                                  :image "/img/dog_house.png")))
                                        (setf doghouse.shootable 1)
                                        (setf doghouse.health 100)
                                        (setf doghouse.shot_image "/img/dog_house_broken.png")
                                        (blocks.push doghouse)))


                                        ; setup, update, and draw are the three main
                                        ; jawsjs functions.

                       setup (lambda ()
                               (setf live_info (gebi "fps"))

                               (setf blocks (new (jaws.-sprite-list)))

                               (setf world (new (jaws.-rect 0 0 3200 640)))

                               (setf blocks-sheet (new (jaws.-sprite-sheet
                                                        (create image "/img/blocks1.png"
                                                                frame_size (array 32 32)
                                                                scale_image 2))))

                               (setf pickups-sheet (new (jaws.-sprite-sheet
                                                         (create image "/img/pickups.png"
                                                                 frame_size (array 34 42)
                                                                 orientation "right"))))


                               (setf tile_map (new (jaws.-tile-map
                                                    (create size (array 1000 1000)
                                                            cell_size (array texture-size texture-size)))))

                               (setf viewport (new (jaws.-viewport (create max_x world.width
                                                                           max_y world.height))))

                               (setf player (new (jaws.-sprite (create x 128
                                                                       y (iv.ground_level)
                                                                       image "/img/daisy.png"
                                                                       anchor "center_bottom"))))
                               (setf player.can_fire "true")
                               (setf player.health 1000)

                               ((@ iv add-scenery))

                               (setf player.vx 0)
                               (setf player.vy 0)
                               (setf player.can_jump true)

                               (tile_map.push (iv.all_blocking))

                               (setf jaws.context.moz-image-smoothing-enabled true)
                               (setf jaws.prevent-default-keys (array "up" "down" "left" "right" "space"))

                               null)



                       player-move (lambda ()
                                     (+= player.x player.vx)
                                     (if (> (@ (tile_map.at-rect (player.rect)) length) 0)
                                         (-= player.x player.vx))
                                     (setf player.vx 0)
                                     (+= player.y player.vy)
                                     (defvar block (aref (tile_map.at-rect (player.rect)) 0))
                                     (if block
                                         (progn
                                           (when (> player.vy 0)
                                             (setf player.can_jump true)
                                             (setf player.y (- (@ (block.rect) y) 1)))
                                           (when (< player.vy 0)
                                             (setf player.y (+ (@ (block.rect) bottom) player.height)))
                                           (setf player.vy 0))))




                                     ;; (when (@ (tile_map.at-rect player.rect) length)
                                     ;;   (progn
                                     ;;     (console.log "collision")
                                     ;;     (setf player.vx 0)))


                                     ;; (when (< (iv.ground_level) player.y)
                                     ;;   (setf player.y (iv.ground_level)))

                                     ;; (defvar block (aref (tile_map.at-rect player.rect) 0))


                                     ;; ;; (when (< player.y 0)
                                     ;; ;;   (+= player.y player.vy)
                                     ;; ;;   (-= player.vy 2))

                                     ;; ;; (when (< player.vy 0)
                                     ;; ;;   (setf player.y (+ (@ (block.rect) bottom) player.height)))

                                     ;;    ; Jumping
                                     ;; ;; (if block
                                     ;; ;;     (progn
                                     ;; ;;       (when (> player.vy 0)
                                     ;; ;;         (console.log "jump")
                                     ;; ;;         (setf player.can_jump true)
                                     ;; ;;         (setf player.y (- (@ (block.rect) y) 1)))
                                     ;; ;;       (when (< player.vy 0)
                                     ;; ;;         (console.log "RESET")
                                     ;; ;;         (setf player.y (+ (@ (block.rect) bottom) player.height)))
                                     ;; ;;       (setf player.vy 0)))

                                     ;;    ; Make sure they stay within view.
                                     ;; (when (and (> player.vx 0)
                                     ;;            (iv.is_outside player))
                                     ;;   (setf player.vx 0))

                                     ;; (+= player.x player.vx)
                                     ;; (+= player.y player.vy)
                                     ;; (setf player.vx 0)
                                     ;; (setf player.vy 0)


                       update (lambda ()
                                (when (jaws.pressed "left")
                                  (unless player.flipped
                                    (player.flip))
                                  (setf player.vx (- 4)))

                                (when (jaws.pressed "right")
                                  (when player.flipped
                                    (player.flip))
                                  (setf player.vx 4))

                                (when (jaws.pressed "up")
                                  (when player.can_jump
                                    (setf player.vy (- 7.5))
                                    (setf player.can_jump false)))

                                (when (jaws.pressed "space")
                                  (iv.fire))

                                ;; move the player
                                (+= player.vy 0.4)
                                (iv.player-move)

                                ; move the other actors
                                (fmap (iv.all_autonymous)
                                      (lambda (x)
                                        (unless x.vx
                                          (setf x.vx -.5))
                                        (when (iv.is_outside x)
                                          ((@ x flip))
                                          (if (> x.vx 0)
                                              (setf x.vx -.5)
                                              (setf x.vx .5)))
                                        (+= x.x x.vx)))

                                (viewport.center-around player)



                                ; Move the bullets
                                (fmap (iv.all_bullets)
                                      (lambda (x)
                                        (+= x.x x.vx)
                                        (+= x.y x.vy)))

                                        ; clean out the bullets that are out of view.
                                (fmap (iv.all_bullets)
                                      (lambda (bullet)
                                        (when (iv.is_outside bullet)
                                          ((@ blocks remove) bullet))))

                                        ; Now check and see if the bullets have hit anything.
                                (fmap (iv.all_bullets)
                                      (lambda (bullet)
                                        (fmap (iv.all_shootable)
                                              (lambda (cbs)
                                                (when ((@ jaws collide-one-with-one) bullet cbs)
                                                  (progn
                                                    (console.log "collision!")
                                                    (iv.handle_collision bullet cbs)))))))

                                ; Finally, remove anything that has lost its life.
                                (fmap blocks.sprites
                                      (lambda (obj)
                                        (when (or (< obj.health 0)
                                                  (= obj.health 0))
                                          (blocks.remove obj))))


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
                                                (player.draw)
                                                (fmap blocks.sprites (lambda (x)
                                                                       ((@ ((@ x.rect)) draw))))
                                                null)))))

                ((@ ($ document) ready) (lambda ()
                                          (jaws.assets.add (array "/img/droid_big.png"
                                                                  "/img/the-hero-small.png"
                                                                  "/img/blocks1.png"
                                                                  "/img/pickups.png"
                                                                  "/img/daisy.png"
                                                                  "/img/dog_house.png"
                                                                  "/img/cherries.png"
                                                                  "/img/roo.png"
                                                                  "/img/rat.png"
                                                                  "/img/factory.png"
                                                                  "/img/dirt.png"
                                                                  "/img/bullet_1.png"
                                                                  "/img/fence-bg.png"
                                                                  "/img/trash_can.png"
                                                                  "/img/coop_1.png"
                                                                  "/img/flowerpot_1.png"
                                                                  "/img/tree_1.png"
                                                                  "/img/daisy_master_coffee.png"
                                                                  "/img/flowerpot_2.png"
                                                                  "/img/wing_pot_1.png"
                                                                  "/img/grass_fg.png"
                                                                  "/img/dog_house_broken.png"
                                                                  "/img/sky_2.png"
                                                                  "/img/bullet.png"
                                                                  "/img/dog-anim.png"))

                                          (jaws.start iv)))))))))
