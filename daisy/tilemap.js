
function Example() {
    var player
    var blocks
    var fps
    var width = 700
    var height = 700
    var tile_map
    var viewport

    /* Called once when a game state is activated. Use it for one-time setup code. */
    this.setup = function() {
        fps = document.getElementById("fps")
        blocks = new jaws.SpriteList()
        
        for(var i = 0; i < width; i++) {
            for(var i2 = 0; i2 < height; i2++) {
                blocks.push( new Sprite({image: "grass.png", x: i*32, y: i2*32}) )
            }
        }
        
        //blocks.push( new Sprite({image: "grass.png", x: 0, y: 0}) )
        viewport = new jaws.Viewport({max_x: width*32, max_y: height*32})

        // A tilemap, each cell is 32x32 pixels. There's 10 such cells across and 10 downwards.
        tile_map = new jaws.TileMap({size: [width, height], cell_size: [32,32]})

        // Fit all items in array blocks into correct cells in the tilemap
        // Later on we can look them up really fast (see player.move)
        tile_map.push(blocks)

        player = new jaws.Sprite({x:10, y:10, scale: 2, anchor: "center"})
        
        var anim = new jaws.Animation({sprite_sheet: "droid_11x15.png", frame_size: [11,15], frame_duration: 100})
        player.anim_default = anim.slice(0,5)
        player.anim_up = anim.slice(6,8)
        player.anim_down = anim.slice(8,10)
        player.anim_left = anim.slice(10,12)
        player.anim_right = anim.slice(12,14)

        player.setImage( player.anim_default.next() )
        jaws.preventDefaultKeys(["up", "down", "left", "right", "space"])
    }

    /* update() will get called each game tick with your specified FPS. Put game logic here. */
    this.update = function() {
        player.setImage( player.anim_default.next() )
        if(jaws.pressed("left"))  { player.move(-2,0);  player.setImage(player.anim_left.next()) }
        if(jaws.pressed("right")) { player.move(2,0);   player.setImage(player.anim_right.next()) }
        if(jaws.pressed("up"))    { player.move(0, -2); player.setImage(player.anim_up.next()) }
        if(jaws.pressed("down"))  { player.move(0, 2);  player.setImage(player.anim_down.next()) }

        viewport.centerAround(player)
        fps.innerHTML = jaws.game_loop.fps + ". player: " + player.x + "/" + player.y
    }

    /* Directly after each update draw() will be called. Put all your on-screen operations here. */
    this.draw = function() {
        jaws.clear()
        /* 
        * blocks & tile_map  = ~500.000 sprites 
        */

        // Slow
        // viewport.apply( function() { blocks.draw(); player.draw(); });

        // Faster:  checks if sprites are with within viewport before calling draw
        // viewport.draw( blocks )


        // Fastest: Use optimized viewport.drawTileMap() */
        viewport.drawTileMap( tile_map )
        viewport.draw( player )
    }
}

$(document).ready(function() { 
    jaws.unpack()
    jaws.assets.add(["droid_11x15.png","block.bmp","grass.png"])
    jaws.start(Example)  // Our convenience function jaws.start() will load assets, call setup and loop update/draw in 60 FPS
});

  
