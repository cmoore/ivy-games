/* Try not to judge - this was generated from parenscript */


var player;
var blocks;
var fps;
var viewport;
var tile_map;
var world;
var show_stats;
var textureSize = 64;
var blocksSheet;
var pickupsSheet;


iv = {
    fire : function () {
        if (player.can_fire === 'true') {
            player.can_fire = 'false';
            var shot = new jaws.Sprite({ image : '/img/bullet.png',
                                         scale : 1,
                                         x : player.flipped ? player.rect().right - 65 : player.rect().right,
                                         y : player.y - 40,
                                         flipped : null,
                                         anchor : 'bottom_center'
                                       });
            shot.vx = player.flipped ? -10 : 10;
            shot.health = 50;
            shot.vy = 0;
            shot.bullet = 1;
            blocks.push(shot);
            setTimeout(function () {
                player.can_fire = 'true';
            }, 500);
        };
    },


    test_trash_can : function () {
        var trashCan = new jaws.Sprite({ image : '/img/trash_can.png',
                                         scale : 1,
                                         x : player.x,
                                         y : player.y,
                                         flipped : null,
                                         anchor : 'bottom_center'
                                       });
        return trashCan.shootable = 1;
    },


    create_rat : function () {
        var newRat = new jaws.Sprite({ image : '/img/rat.png',
                                       scale : 1.5,
                                       x : player.x - 128,
                                       y : player.y,
                                       flipped : null,
                                       anchor : 'bottom_center'
                                     });
        newRat.autonymous = 1;
        newRat.shootable = 1;
        newRat.health = 100;
        return blocks.push(newRat);
    },


    create_chicken_black : function () {
        var newChicken = new jaws.Sprite({ image : '/img/roo.png',
                                           scale : 1,
                                           x : player.x - 128,
                                           y : player.y,
                                           flipped : null,
                                           anchor : 'bottom_center'
                                         });
        newChicken.autonymous = 1;
        newChicken.shootable = 1;
        newChicken.health = 200;
        return blocks.push(newChicken);
    },


    draw_text : function (text) {
        jaws.context.font = '20pt Arial';
        jaws.context.lineWidth = 1;
        jaws.context.fillStyle = 'red';
        jaws.context.strokeStyle = 'rgba(200,200,200,0.0)';
        return jaws.context.fillText(text, 10, 10);
    },


    add_auto : function () {
        var ohgod = new jaws.Sprite({ image : 'roo.png',
                                      scale : 1,
                                      x : player.x,
                                      y : world.height - 128,
                                      flipped : null,
                                      anchor : 'bottom_center'
                                    });
        ohgod.autonymous = 1;
        blocks.push(ohgod);
    },


    all_shootable : function () {
        return _.filter(blocks.sprites, function (x) {
            return x.shootable === 1;
        });
    },


    all_autonymous : function () {
        return _.filter(blocks.sprites, function (x) {
            return x.autonymous === 1;
        });
    },

    all_bullets : function () {
        return _.filter(blocks.sprites, function (x) {
            return x.bullet === 1;
        });
    },


    all_blocking : function () {
        return _.filter(blocks.sprites, function (x) {
            return x.blocking === 1;
        });
    },


    ground_level : function () {
        return world.height - 5;
    },


    is_outside : function (o) {
        o.x < 0 || o.x > world.width;
    },

    handle_collision : function (bullet, target) {
        target.health -= bullet.health;
        console.log(target.health);
        if (target.shot_image) {
            target.setImage(target.shot_image);
        };
        blocks.remove(bullet);
    },


    dropPowerups : function () {
        for (var i = 0; i < 20; i += 1) {
            var g28 = new jaws.Sprite({ image : '/img/dirt.png',
                                        scale : 1,
                                        x : 30 + 32 * i,
                                        y : world.height - 2 * textureSize,
                                        flipped : null,
                                        anchor : 'bottom_center'
                                      });
            g28.setImage(pickupsSheet.frames[i]);
            powerups.push(g28);
            blocks.push(g28);
        };
    },

    randomTiles : function () {
        for (var i = 0; i < 100; i += 1) {
            var rx = textureSize * parseInt(100 * Math.random());
            var ry = world.height - textureSize * parseInt(10 * Math.random());
            blocks.push(new jaws.Sprite({ image : '/img/dirt.png',
                                          scale : 1,
                                          x : rx,
                                          y : ry,
                                          flipped : null,
                                          anchor : 'bottom_center'
                                        }));
        };
    },


    addRoo : function () {
        var theRoo = new jaws.Sprite({ image : '/img/roo.png',
                                       scale : 1,
                                       x : 512,
                                       y : world.height - 2 * textureSize,
                                       flipped : null,
                                       anchor : 'bottom_center'
                                     });
        iv.make_autonymous(theRoo);
        iv.make_shootable(theRoo);
        blocks.push(theRoo);
    },


    addScenery : function () {
        var i = 0;
        for (; i <= world.width; ) {
            var newBlock = new jaws.Sprite({ image : '/img/dirt.png',
                                             scale : 1,
                                             x : i,
                                             y : iv.ground_level(),
                                             flipped : null,
                                             anchor : 'top_left'
                                           });
            newBlock.blocking = 1;
            blocks.push(newBlock);
            var _js3 = i + textureSize;
            i = _js3;
        };
        iv.add_doghouse('x', 512, 'y', iv.ground_level());
    },

    add_doghouse : function () {
        var _js4 = arguments.length;
        for (var n3 = 0; n3 < _js4; n3 += 2) {
            switch (arguments[n3]) {
            case 'x':
                x = arguments[n3 + 1];
                break;
            case 'y':
                y = arguments[n3 + 1];
            };
        };
        var x = 'undefined' === typeof x ? 0 : x;
        var y = 'undefined' === typeof y ? 0 : y;
        var doghouse = new jaws.Sprite({ image : '/img/dog_house.png',
                                         scale : 0.5,
                                         x : x,
                                         y : y,
                                         flipped : null,
                                         anchor : 'bottom_center'
                                       });
        doghouse.shootable = 1;
        doghouse.health = 100;
        doghouse.shot_image = '/img/dog_house_broken.png';
        blocks.push(doghouse);
    },

    setup : function () {
        live_info = document.getElementById('fps');
        blocks = new jaws.SpriteList();
        world = new jaws.Rect(0, 0, 3200, 640);
        blocksSheet = new jaws.SpriteSheet({ image : '/img/blocks1.png',
                                             frame_size : [32, 32],
                                             scale_image : 2
                                           });
        pickupsSheet = new jaws.SpriteSheet({ image : '/img/pickups.png',
                                              frame_size : [34, 42],
                                              orientation : 'right'
                                            });
        tile_map = new jaws.TileMap({ size : [1000, 1000], cell_size : [textureSize, textureSize] });
        viewport = new jaws.Viewport({ max_x : world.width, max_y : world.height });
        player = new jaws.Sprite({ x : 128,
                                   y : iv.ground_level(),
                                   image : '/img/daisy.png',
                                   anchor : 'center_bottom'
                                 });
        player.can_fire = 'true';
        player.health = 1000;
        iv.addScenery();
        player.vx = 0;
        player.vy = 0;
        player.can_jump = true;
        tile_map.push(iv.all_blocking());
        jaws.context.mozImageSmoothingEnabled = true;
        jaws.preventDefaultKeys = ['up', 'down', 'left', 'right', 'space'];
    },


    playerMove : function () {
        player.x += player.vx;
        if (tile_map.atRect(player.rect()).length > 0) {
            player.x -= player.vx;
        }
        player.vx = 0;
        player.y += player.vy;

        var block = tile_map.atRect(player.rect())[0];
        if (block) {
            if (player.vy > 0) {
                player.can_jump = true;
                player.y = block.rect().y - 1;
            };
            if (player.vy < 0) {
                player.y = block.rect().bottom + player.height;
            };
            return player.vy = 0;
        };
    },


    update : function () {
        if (jaws.pressed('left')) {
            if (!player.flipped) {
                player.flip();
            };
            player.vx = -4;
        };
        if (jaws.pressed('right')) {
            if (player.flipped) {
                player.flip();
            };
            player.vx = 4;
        };
        if (jaws.pressed('up')) {
            if (player.can_jump) {
                player.vy = -7.5;
                player.can_jump = false;
            };
        };
        if (jaws.pressed('space')) {
            iv.fire();
        };
        player.vy += 0.4;
        iv.playerMove();
        _.map(iv.all_autonymous(), function (x) {
            if (!x.vx) {
                x.vx = -0.5;
            };
            if (iv.is_outside(x)) {
                x.flip();
                if (x.vx > 0) {
                    x.vx = -0.5;
                } else {
                    x.vx = 0.5;
                };
            };
            return x.x += x.vx;
        });
        viewport.centerAround(player);
        _.map(iv.all_bullets(), function (x) {
            x.x += x.vx;
            return x.y += x.vy;
        });
        _.map(iv.all_bullets(), function (bullet) {
            return iv.is_outside(bullet) ? blocks.remove(bullet) : null;
        });
        _.map(iv.all_bullets(), function (bullet) {
            return _.map(iv.all_shootable(), function (cbs) {
                if (jaws.collideOneWithOne(bullet, cbs)) {
                    console.log('collision!');
                    return iv.handle_collision(bullet, cbs);
                };
            });
        });
        _.map(blocks.sprites, function (obj) {
            return obj.health < 0 || obj.health === 0 ? blocks.remove(obj) : null;
        });
        show_stats && (live_info.innerHTML = jaws.game_loop.fps + 'fps. P: ' + parseInt(player.x) + '/' + parseInt(player.y) + '  PV ' + parseInt(player.vx) + '/' + parseInt(player.vy) + '  Viewport: ' + parseInt(viewport.x) + '/' + parseInt(viewport.y) + ' World: ' + parseInt(world.width) + '/' + parseInt(world.height));
    },


    draw : function () {
        jaws.clear();
        return viewport.apply(function () {
            blocks.draw();
            player.draw();
            _.map(blocks.sprites, function (x) {
                return x.rect().draw();
            });
            return null;
        });
    }
};

$(document).ready(function () {
    jaws.assets.add(['/img/droid_big.png',
                     '/img/the-hero-small.png',
                     '/img/blocks1.png',
                     '/img/pickups.png',
                     '/img/daisy.png',
                     '/img/dog_house.png',
                     '/img/cherries.png',
                     '/img/roo.png',
                     '/img/rat.png',
                     '/img/factory.png',
                     '/img/dirt.png',
                     '/img/bullet_1.png',
                     '/img/fence-bg.png',
                     '/img/trash_can.png',
                     '/img/coop_1.png',
                     '/img/flowerpot_1.png',
                     '/img/tree_1.png',
                     '/img/daisy_master_coffee.png',
                     '/img/flowerpot_2.png',
                     '/img/wing_pot_1.png',
                     '/img/grass_fg.png',
                     '/img/dog_house_broken.png',
                     '/img/sky_2.png',
                     '/img/bullet.png',
                     '/img/dog-anim.png']);
    return jaws.start(iv);
});
