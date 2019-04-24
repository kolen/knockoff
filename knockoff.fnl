;; script: fennel

(var stage :intro)

(var player {:pos [2 2]
             :anim :idle})

(var entities {})
(var entities-last-id 0)
(var entities-enqueued {})

(local keybindings
       {:left 1
        :right 4
        :top 23
        :bottom 19})

(local tile-ids
       {:space 0
        :ground 1
        :boulder 4
        :gem -1 ; todo
        :wall -1 ; todo
        :player 16
        :player-side 17})

(local directions
       {:left   [-1  0]
        :right  [ 1  0]
        :top    [ 0 -1]
        :bottom [ 0  1]})

(local sounds
       {:noise 0
        :boulder 1})

(local player-anim-frame-end-move 16)
(local player-anim-frame-move 4)

(fn entity-create [data]
  (set entities-last-id (+ entities-last-id 1))
  ;; we can't create new entities while iterating over entities list
  (tset entities-enqueued entities-last-id data))

(fn entity-delete [id]
  (tset entities id nil))

(fn entity-flush-enqueued []
  (each [id data (pairs entities-enqueued)] (tset entities id data))
  (set entities-enqueued {}))

(fn fallible-slippery? [tile-id]
  (or (= tile-id tile-ids.boulder)
      (= tile-id tile-ids.gem)
      (= tile-id tile-ids.wall)))

(fn fallible-fallible? [tile-id]
  (or (= tile-id tile-ids.boulder)
      (= tile-id tile-ids.gem)))

;; fall fallible at x, y (coordinates of actual fallible, not empty
;; space, unlike fallible-check-fall)
(fn fallible-fall [x y targetx]
  (trace (.. "fall " x " " targetx " " y))
  (if (= targetx x)
      (entity-create {:fallible (mget x y) :x x :y y :frames 0})
      (entity-create {:fallible (mget x y) :x x :y y :targetx targetx :frames 0})))

;; check fall directly, assuming x, y has space at the moment of call
(fn fallible-check-direct-fall [x y]
  (when (fallible-fallible? (mget x (- y 1)))
    (fallible-fall x (- y 1) x)))

;; check fall on slippery block at x, y
(fn fallible-check-slippery-fall [x y holex]
  (when (and (fallible-slippery? (mget x y))
             (fallible-fallible? (mget x (- y 1)))
             (= (mget holex y) tile-ids.space))
    (fallible-fall x (- y 1) holex)))

;; check that fallible can fall after making space at x, y (assuming
;; that there's space at x, y at the time of call), creating actual
;; fallible if it falls
(fn fallible-check-fall [x y]
  (fallible-check-slippery-fall (- x 1) y x)
  (fallible-check-slippery-fall (+ x 1) y x)
  (fallible-check-slippery-fall (+ x 1) (+ y 1) x)
  (fallible-check-slippery-fall (- x 1) (+ y 1) x)
  (fallible-check-direct-fall x y))

(local fallible-step-period 32)

(fn make-space [x y]
  (mset x y tile-ids.space)
  (fallible-check-fall x y))

(fn fallible-step [entity-id]
  (let [fallible (. entities entity-id)]
    ;; destructuring would be handy but it might be broken in stable
    ;; tic-80, TODO: check if it works
    (if (. fallible :targetx)
        ;; Falling on the side (slippering)
        (if (= (mget (. fallible :targetx) (. fallible :y)) tile-ids.space)
            ;; Move to slipping direction
            (do
              (make-space (. fallible :x) (. fallible :y))
              (mset (. fallible :targetx) (. fallible :y) (. fallible :fallible))
              (tset fallible :x (. fallible :targetx))
              (tset fallible :targetx nil)
              (tset fallible :frames 0))
            ;; No space to move left - just stay there, entity not
            ;; needed anymore
            (entity-delete entity-id))

        ;; Falling down
        (= (mget (. fallible :x) (+ 1 (. fallible :y))) tile-ids.space)
        (do (make-space (. fallible :x) (. fallible :y))
            (mset (. fallible :x) (+ 1 (. fallible :y)) (. fallible :fallible))
            (tset fallible :frames 0)
            (tset fallible :y (+ 1 (. fallible :y))))

        ;; No more place to fall
        (do
          (entity-delete entity-id)
          ;; FIXME: do it correctly, currently it duplicates blocks
          (fallible-check-fall (- 1 (. fallible :x)) (+ 1 (. fallible :y)))
          (fallible-check-fall (+ 1 (. fallible :x)) (+ 1 (. fallible :y)))
          (sfx sounds.boulder 29 30 2 10) ;; FIXME: sound should play
                                          ;; right after falling down,
                                          ;; not here
          ))))

(fn fallible-update [entity-id]
  (let [fallible (. entities entity-id)]
    (tset fallible :frames (+ 1 (. fallible :frames)))
    (if (= fallible-step-period (. fallible :frames))
        (fallible-step entity-id))))

(fn player-start-move [dir]
  (set player.anim {:dir dir :frames 0}))

(fn player-check-movement []
  (if (key keybindings.left)   (player-start-move :left)
      (key keybindings.right)  (player-start-move :right)
      (key keybindings.top)    (player-start-move :top)
      (key keybindings.bottom) (player-start-move :bottom)
      (set player.anim :idle)))

(fn player-move [dir]
  (let [[x y] player.pos
        [xdiff ydiff] (. directions dir)
        [newx newy] [(+ x xdiff) (+ y ydiff)]]
    (make-space x y)
    (mset newx newy tile-ids.player)
    (set player.pos [newx newy])))

(fn player-start-push-boulder []
  (set player.anim.push true))

(fn player-try-move [dir]
  (let [[x y] player.pos
        [xdiff ydiff] (. directions dir)
        [newx newy] [(+ x xdiff) (+ y ydiff)]
        target-tile (mget newx newy)]
    (if (= target-tile tile-ids.space)
        (do (sfx sounds.noise 48 2 0 1)
            (player-move dir))
        (= target-tile tile-ids.ground)
        (do (sfx sounds.noise 32 4 0 1)
            (player-move dir))
        (and (= target-tile tile-ids.boulder)
             (or (= dir :left) (= dir :right)))
        (player-start-push-boulder))))

(fn update-player []
  (if (= player.anim :idle)
      (player-check-movement)
      (do
        (set player.anim.frames (+ 1 player.anim.frames))
        (if (= player.anim.frames player-anim-frame-move) (player-try-move player.anim.dir)
            ;; if it's last animation frame, check if movement
            ;; continues. This will set animation to idle if no movement
            ;; controls touched.
            (= player.anim.frames player-anim-frame-end-move) (player-check-movement)))))

(fn update-entities []
  (each [id data (pairs entities)]
    (if (. data :fallible) (fallible-update id)))
  (entity-flush-enqueued))

(fn game-level-start []
  (for [x 0 30]
    (for [y 0 16]
      (when (= (mget x y) (. tile-ids :player))
        (set player.pos [x y])))))

(fn on-frame-intro []
  (cls)
  (print "intro" 10 10)
  (when (key 48)
    (set stage :game)
    (game-level-start)))

(fn tiles-remap [tile-id x y]
  (if (= tile-id tile-ids.player)
      (if (= player.anim :idle) tile-id
          (let [sprite-num (% (// player.anim.frames 4) 3)
                new-tile-id (+ tile-ids.player-side sprite-num)
                flip (if (or (= player.anim.dir :left) (= player.anim.dir :top)) 1 0)]
          (values new-tile-id flip)))
      tile-id))

(fn on-frame-game []
  (update-player)
  (update-entities)
  (cls)
  (map 0 0 30 17 0 0 -1 1 tiles-remap))

(fn on-frame []
  (if (= stage :intro)
      (on-frame-intro)
      (= stage :game)
      (on-frame-game)))

(global TIC on-frame)
