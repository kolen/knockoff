;; script: fennel

(var player {:pos [2 2]
             :anim :idle})

(var entities {})
(var entities-last-id 0)
(var entities-enqueued {})

(var game {:frame 0
           :gems {:to-eat 32
                  :eaten 0}
           :camera [0 0]
           :camera-frame 0
           :level-boundaries [0 0 (* 30 2) (* 16 2)]})

(local keybindings
       {:left 1
        :right 4
        :top 23
        :bottom 19})

(local tile-ids
       {:space 0
        :ground 1
        :wall 3
        :boulder 4
        :gem 5
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

;; check if fallible should continue to fall, or stop, or start
;; sliding to the side
(fn fallible-check-continue [entity-id]
  (let [fallible (. entities entity-id)
        tile-id-below (mget (. fallible :x)
                            (+ 1 (. fallible :y)))
        tile-id-slip-right (mget (+ 1 (. fallible :x))
                                 (+ 1 (. fallible :y)))
        tile-id-slip-left (mget (- 1 (. fallible :x))
                                (+ 1 (. fallible :y)))]
    (when (~= tile-id-below tile-ids.space)
      (sfx sounds.boulder 29 30 2 10)
      (if (and (fallible-slippery? tile-id-below) (= tile-id-left tile-ids.space))
          (do (tset fallible :targetx (- (. fallible :x) 1))
              (tset fallible :frames 0))
          (and (fallible-slippery? tile-id-below) (= tile-id-right tile-ids.space))
          (do (tset fallible :targetx (+ (. fallible :x) 1))
              (tset fallible :frames 0))
          (entity-delete entity-id)))))

(fn fallible-step [entity-id]
  (let [fallible (. entities entity-id)]
    ;; destructuring would be handy but it might be broken in stable
    ;; tic-80, TODO: check if it works
    (if (~= (. fallible :fallible) (mget (. fallible :x) (. fallible :y)))
        ;; Fallible might be eaten/killed by explosion "concurrently"
        (entity-delete entity-id)

        ;; Falling on the side (slippering)
        (. fallible :targetx)
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
            (tset fallible :y (+ 1 (. fallible :y)))
            (fallible-check-continue entity-id))

        ;; No more place to fall
        (entity-delete entity-id))))

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

(fn player-gems-reached []
  (trace "Number of gems reached"))

(fn player-eat-gem []
  (sfx sounds.noise 64 4 0 1); FIXME: correct gem sound
  (set game.gems.eaten (+ 1 (. game.gems.to-eat)))
  (when (>= game.gems.eaten game.gems.to-eat)
    (player-gems-reached)))

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
        (= target-tile tile-ids.gem)
        (do (player-eat-gem)
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

(fn map-tiles-remap [tile-id x y]
  (if (= tile-id tile-ids.player)
      (if (= player.anim :idle) tile-id
          (let [sprite-num (% (// player.anim.frames 4) 3)
                new-tile-id (+ tile-ids.player-side sprite-num)
                flip (if (or (= player.anim.dir :left) (= player.anim.dir :top)) 1 0)]
            (values new-tile-id flip)))
      (= tile-id tile-ids.gem)
      (+ tile-id (% (// game.frame 3) 4))
      tile-id))

(fn map-camera-follow []
  (when (= (% game.frame 30) 0)
    (let [[cam-x cam-y] game.camera
          screen-w 30
          screen-h 17
          [player-x player-y] player.pos
          x-lim-min (+ cam-x 6)
          x-lim-max (+ cam-x (- screen-w 6))
          y-lim-min (+ cam-y 4)
          y-lim-max (+ cam-y (- screen-h 4))
          [lvl-x-a lvl-y-a lvl-x-b1 lvl-y-b1] game.level-boundaries
          [lvl-x-b lvl-y-b] [(- lvl-x-b1 screen-w)
                             (- lvl-y-b1 screen-h)]
          [speed-x speed-y] [(if (< player-x x-lim-min) -1
                                 (> player-x x-lim-max) 1
                                 0)
                             (if (< player-y y-lim-min) -1
                                 (> player-y y-lim-max) 1
                                 0)]
          new-cam-x1 (+ cam-x speed-x)
          new-cam-y1 (+ cam-y speed-y)
          new-cam-x (if (< new-cam-x1 lvl-x-a) lvl-x-a
                        (> new-cam-x1 lvl-x-b) lvl-x-b
                        new-cam-x1)
          new-cam-y (if (< new-cam-y1 lvl-y-a) lvl-y-a
                        (> new-cam-y1 lvl-y-b) lvl-y-b
                        new-cam-y1)]
      (tset game :camera [new-cam-x new-cam-y]))))

(fn map-render []
  (let [[cam-x cam-y] game.camera]
    (map cam-x cam-y
         30 16 ;; width and height
         0 8   ;; skip first line, which contain score, etc
         -1    ;; transparent color
         1     ;; scale
         map-tiles-remap)))

(fn game-on-frame []
  (set game.frame (% (+ 1 game.frame) 60))
  (update-player)
  (update-entities)
  (map-camera-follow)
  (cls)
  (map-render))

(var intro {:frame 0
            :glitter nil
            :glitter-frame 0})

(fn intro-on-frame []
  (set intro.frame (% (+ 1 intro.frame) 30))
  (when (= intro.frame 0)
    (set intro.glitter [(math.random 64 80) (math.random 40 56)])
    (set intro.glitter-frame 0))
  (cls)
  (fn draw-wall [wall-x flip]
    (let [wall-y 8]
      (spr 160 wall-x wall-y -1 1 flip 0 2 2)
      (for [y 0 3] (spr 192 wall-x (+ wall-y (* y 16)) -1 1 flip 0 2 2))
      (spr 224 wall-x (+ wall-y (* 16 4)) -1 1 flip 0 2 2)))
  ;; walls
  (draw-wall 16 0)
  (draw-wall (- 240 32) 1)
  ;; floor
  (for [x 4 25] (spr 248 (* 8 x) 80))
  ;; logo
  (spr 96 112 32 -1 1 0 0 10 2)
  ;; froggy
  (spr 137 32 24 -1 1 0 0 7 8)
  ;; text
  (print "Press space to continue" 56 112 10)
  (when intro.glitter
    (let [[glitter-x glitter-y] intro.glitter]
      (if (< intro.glitter-frame 9)
          (do (spr (+ 245 (// intro.glitter-frame 3)) glitter-x glitter-y 0)
              (set intro.glitter-frame (+ 1 intro.glitter-frame)))
          (set intro.glitter nil))))
  (when (key 48)
    (global TIC game-on-frame)
    (game-level-start)))

(global TIC intro-on-frame)
