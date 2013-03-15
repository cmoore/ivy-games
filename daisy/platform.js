function platform() {
    var player
    var blocks
    var fps
    var viewport
    var tile_map

    /* Called once when a game state is activated.
       Use it for one-time setup code. */
    this.setup = function() {
        live_info = document.getElementById("jaws-log")
        blocks = new jaws.SpriteList();
        var world = new jaws.Rect(0,0,3200,640);

        /* We create some 32x32 blocks and save them in array blocks */
        for(var y = 0; y < world.height; y += 32 ) {
            blocks.push( new Sprite({image: "block.bmp", x: 0, y: y}) );

            blocks.push( new Sprite({image: "block.bmp",
                                     x: world.width-32,
                                     y: y}) );
            }
            for(var x = 0; x < world.width; x += 32 ) {
                blocks.push( new Sprite({image: "block.bmp", x: x, y: world.height-32}) )
            }
            for(var x = 320; x < world.width; x += 32 ) {
                blocks.push( new Sprite({image: "block.bmp", x: x, y: world.height-32}) )
            }
            for(var i=0; i < 100; i++) {
                blocks.push( new Sprite({
                    image: "block.bmp",
                    x: parseInt(Math.random()*100)*32,
                    y: world.height - parseInt(Math.random()*10)*32}))
            }

            // A tile map, each cell is 32x32 pixels. There's 100 such cells across and 100 downwards.
            // Fit all items in array blocks into correct cells in the tilemap
            // Later on we can look them up really fast (see player.move)
            tile_map = new jaws.TileMap({size: [100,100], cell_size: [32,32]})
            tile_map.push(blocks)

            viewport = new jaws.Viewport({max_x: world.width, max_y: world.height})

            player = new jaws.Sprite({x:110, y:320, scale: 1.5, anchor: "center_bottom"})

            player.move = function() {
                this.x += this.vx
                if(tile_map.atRect(player.rect()).length > 0) {
                    this.x -= this.vx
                }
                this.vx = 0

                this.y += this.vy
                var block = tile_map.atRect(player.rect())[0]
                if(block) {
                    // Heading downwards
                    if(this.vy > 0) {
                        this.can_jump = true
                        this.y = block.rect().y - 1
                    }
                    // Heading upwards (jumping)
                    if(this.vy < 0) {
                        this.y = block.rect().bottom + this.height
                    }
                    this.vy = 0
                }
            }

            var anim = new jaws.Animation({sprite_sheet: "/img/droid_11x15.png", frame_size: [11,15], frame_duration: 100})

            player.anim_default = anim.slice(0,5)
            player.anim_up = anim.slice(6,8)
            player.anim_down = anim.slice(8,10)
            player.anim_left = anim.slice(10,12)
            player.anim_right = anim.slice(12,14)
            player.vx = player.vy = 0
            player.can_jump = true

            player.setImage( player.anim_default.next() )
            jaws.context.mozImageSmoothingEnabled = true;  // non-blurry, blocky retro scaling
            jaws.preventDefaultKeys(["up", "down", "left", "right", "space"])
        }

        /* update() will get called each game tick with your specified FPS. Put game logic here. */

        this.update = function() {
            player.setImage( player.anim_default.next() )

            player.vx = 0
            if(jaws.pressed("left"))  {
                player.vx = -2;
                player.setImage(player.anim_left.next());
            }
            if(jaws.pressed("right")) {
                player.vx = 2;
                player.setImage(player.anim_right.next())
            }
            if(jaws.pressed("up")) {
                if(player.can_jump) {
                    player.vy = -7.5; player.can_jump = false
                }
            }

            // some gravity
            player.vy += 0.4

            // apply vx / vy (x velocity / y velocity), check for collision detection in the process.
            player.move()

            // Tries to center viewport around player.x / player.y.
            // It won't go outside of 0 or outside of our previously specified max_x, max_y values.
            viewport.centerAround(player)

            live_info.innerHTML = jaws.game_loop.fps + " fps. Player: " + parseInt(player.x) + "/" + parseInt(player.y) + ". "
            live_info.innerHTML += "Viewport: " + parseInt(viewport.x) + "/" + parseInt(viewport.y) + "."
        };

        /* Directly after each update draw() will be called. Put all your on-screen operations here. */
        this.draw = function() {
            jaws.clear()

            // the viewport magic. wrap all draw()-calls inside viewport.apply and it will draw those relative to the viewport.
            viewport.apply( function() {
                blocks.draw()
                player.draw()
            });
        }
    }

jaws.onload = function() {
    jaws.unpack()
    jaws.assets.add(["/img/droid_11x15.png","block.bmp"])
    jaws.start(platform)  // Our convenience function jaws.start() will load assets, call setup and loop update/draw in 60 FPS
}
