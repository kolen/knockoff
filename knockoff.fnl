;; script: fennel

(var stage :intro)

(var player {:pos [2 2]
             :anim :idle})

(local keybindings
       {:left 1
        :right 4
        :top 23
        :bottom 19})

(local tile-ids
       {:space 0
        :ground 1
        :boulder 4
        :player 16})

(local directions
       {:left   [-1  0]
        :right  [ 1  0]
        :top    [ 0 -1]
        :bottom [ 0  1]})

(local player-anim-frame-end-move 16)
(local player-anim-frame-move 4)

(fn player-start-move [dir]
  (set player.anim {:dir dir :frames 0}))

(fn player-check-movement []
  (when (key keybindings.left)   (player-start-move :left))
  (when (key keybindings.right)  (player-start-move :right))
  (when (key keybindings.top)    (player-start-move :top))
  (when (key keybindings.bottom) (player-start-move :bottom)))

(fn player-move [dir]
  (let [[x y] player.pos
        [xdiff ydiff] (. directions dir)
        [newx newy] [(+ x xdiff) (+ y ydiff)]]
    (mset x y tile-ids.space)
    (mset newx newy tile-ids.player)
    (set player.pos [newx newy])))

(fn player-start-push-boulder []
  (set player.anim.push true))

(fn player-try-move [dir]
  (let [[x y] player.pos
        [xdiff ydiff] (. directions dir)
        [newx newy] [(+ x xdiff) (+ y ydiff)]
        target-tile (mget newx newy)]
    (when (or (= target-tile tile-ids.ground)
              (= target-tile tile-ids.space))
      (player-move dir))
    (when (and (= target-tile tile-ids.boulder)
               (or (= dir :left) (= dir :right)))
      (player-start-push-boulder))))

(fn update-player []
  (match player.anim
    :idle ;; idle - allow to move
    (player-check-movement)
    {:dir dir :frames frames} ;; moving - handle it
    (do
      (set player.anim.frames (+ 1 player.anim.frames))
      (if (= frames player-anim-frame-move) (player-try-move dir)
          (= frames player-anim-frame-end-move) (set player.anim :idle)))))

(fn game-level-start []
  (for [x 0 30]
    (for [y 0 16]
      (when (= (mget x y) (. tile-ids :player))
        (set player.pos [x y])
        (trace (.. "set player pos " x ", " y))))))

(fn on-frame-intro []
  (cls)
  (print "intro" 10 10)
  (when (key 48)
    (set stage :game)
    (game-level-start)))

(fn on-frame-game []
  (update-player)
  (cls)
  (map))

(fn on-frame []
  (match stage
    :intro (on-frame-intro)
    :game (on-frame-game)))

(global TIC on-frame)
