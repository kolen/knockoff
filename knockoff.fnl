;; script: fennel

(var stage :intro)
(var game
     {:player {:pos [2 2]
               :anim :idle}
      })

(local keybindings
       {:left 1
        :right 4
        :top 23
        :bottom 19})

(local tile-ids
       {:player 16
        :space 15})

(local directions
       {:left   [-1  0]
        :right  [ 1  0]
        :top    [ 0 -1]
        :bottom [ 0  1]})

(local player-anim-frames-per-move 15)

(fn player-try-move [dir]
  (let [[x y] game.player.pos
        [xdiff ydiff] (. directions dir)
        [newx newy] [(+ x xdiff) (+ y ydiff)]]
    (mset x y tile-ids.space)
    (mset newx newy tile-ids.player)
    (set game.player.pos [newx newy])
    (set game.player.anim {:dir dir :frames player-anim-frames-per-move})))

(fn player-check-movement []
  (when (key keybindings.left)   (player-try-move :left))
  (when (key keybindings.right)  (player-try-move :right))
  (when (key keybindings.top)    (player-try-move :top))
  (when (key keybindings.bottom) (player-try-move :bottom)))

(fn update-player []
  (match game.player.anim
    :idle ;; idle - allow to move
    (player-check-movement)
    {:dir dir :frames frames} ;; moving - handle it
    (do (set game.player.anim.frames (- game.player.anim.frames 1))
        (when (<= game.player.anim.frames 0) (set game.player.anim :idle)))))

(fn game-level-start []
  (for [x 0 30]
    (for [y 0 16]
      (when (= (mget x y) (. tile-ids :player))
        (set game.player.pos [x y])
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
