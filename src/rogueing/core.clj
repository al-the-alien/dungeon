(ns rogueing.core
    (:require
     [lanterna.terminal :as t]
     [lanterna.screen :as s]
     [clojure.pprint :refer [pprint]]))


(def scr (s/get-screen :swing))

(defn apply-or
    [coll]
    (some identity coll))


;;-----------------Rooms---------------

;;----Creating rooms----

(defn calculate-room-specs
    "Returns a map of specs for a room, given its height width and offsets."
    [room-width room-height x-offset y-offset]
    (let [last-x (+ room-width x-offset)
          last-y (+ room-height y-offset)]
        {:first-x x-offset
         :last-x last-x
         :first-y y-offset
         :last-y last-y
         :width room-width
         :height room-height
         :center [(- last-x (int (/ room-width 2)))
                  (- last-y (int (/ room-height 2)))]}))


;; Two versions of create-room are provided: one which just returns the
;;    coordinates for the room, and one which returns the coordinates
;;    subdevided by row.
;;    In this version, the pad-room function only works if the room
;;    is devided by row.
(defn create-room
    "Returns a list of coordinates for the room, given its specifications."
    [room-specs]

    ;; This code will returns a list of coordinates
    ;; (for [x (range (:first-x room-specs) (inc (:last-x room-specs)))
    ;;       y (range (:first-y room-specs) (inc (:last-y room-specs)))]
    ;;     (vector x y))

    ;; This code returns a list of coordinates devided by row.
    (for [x (range (:first-x room-specs) (inc (:last-x room-specs)))]
        (let [ys (range (:first-y room-specs) (inc (:last-y room-specs)))]
            (mapv #(vector x %) ys)))
   )


;; ---Checking for room intersection---

(defn pad-room
    "Adds an extra row/collumn to each side of the room."
    [room]
    (let [padded  (concat room

                          ;; One collumn before room
                          (for [[x y] (first room)]
                              [(dec x) y])

                          ;; One collumn after room
                          (for [[x y] (last room)]
                              [(inc x) y])

                          ;; One row above room
                          (for [[x y] (map first room)]
                              [x (dec y)])

                          ;; One row below room
                          (for [[x y] (map last room)]
                              [x (inc y)])
                          )]
        (partition 2 (flatten padded))))


(defn in-room?
    "returns true if coord exists in the room"
    [coord room]
    (apply-or (for [row room]
                  (some #(= % coord) row))))


(defn intersects?
    "returns true if room overlaps with any of the pre-existing-rooms"
    [room pre-existing-rooms]
    (let [room (pad-room room)]
        (apply-or (for [coord room
                        r pre-existing-rooms]
                      (in-room? coord r)))))



;;---Generating rooms for a level---

;; FIXME: generate-rooms can end up in an infinite loop (if so many
;;     rooms are generated that it is impossible to generate another without
;;     intersection).
(defn generate-rooms
    "Returns a list of rooms for one level of the map. Checks to be sure that
none of the rooms overlap."
    [n-rooms]
    (loop [countdown n-rooms
           rooms []]
        (let [width (rand-nth (range 4 14))
              height (rand-nth (range 4 8))
              offset-x (rand-nth (range 1 65))
              offset-y (rand-nth (range 1 18))

              room-specs (calculate-room-specs width height offset-x offset-y)
              room (create-room room-specs)]

            (if (intersects? room (map first rooms))
                (recur countdown rooms)
                (if (< 0 countdown)
                    (recur (dec countdown)
                           (conj rooms [room room-specs]))
                    rooms)))))




;;----------------------Corriders----------------

(defn create-corrider
    [start-coords end-coords]
    (let [[start-x start-y] start-coords
          [end-x end-y] end-coords
          [x1 x2] (sort [start-x end-x])
          [y1 y2] (sort [start-y end-y])]
        (into
         ;; horizontal section
         (for [x (range x1 (inc x2))]
             [x start-y])

         ;; vertical section
         (for [y (range y1 (inc y2))]
             [end-x y]))))


;;---Generate the corriders for a level---

(defn generate-corriders
    "Returns a collection of corriders based on the current layout of rooms"
    [centers]
    (loop [to-connect centers
           corriders []]
        (if (<= 2 (count to-connect))
            (recur (rest to-connect)
                   (conj corriders
                         (create-corrider (first to-connect)
                                          (second to-connect))))
            corriders)))



;;--------------------Screen printing------------------

(defn pre-excavated-map
    [scr]
    (let [[screen-width screen-height] (s/get-size scr)]
        (for [y (range screen-height)]
            (let [line (apply str (repeat screen-width "#"))]
                (s/put-string scr 0 y line)))))


(defn print-map
    [coords scr]
    (for [[x y] coords]
        (s/put-string scr x y " ")))


;; FIXME: pre-excavated-map doesn't appear to be doing anything (it should be
;;   printing "#" at every place in the screen). Figure out what's up with that
;;   and how to fix it, so pre-excavated-map doesn't have to be called
;;   seperately.
(defn print-screen
    [scr n]
    (pre-excavated-map scr)
    (let [rooms (generate-rooms n)
          room-coords (map first rooms)
          all-specs (map last rooms)
          centers (map :center all-specs)
          corriders (generate-corriders centers)
          map (into room-coords corriders)
          map-coords (partition 2 (flatten map))]

        (print-map map-coords scr)))


;;------------Running the program---------------

(comment
    ;; First you mush open a screen
    (s/start scr)

    ;; Execute each line sequentially to create a dungeon.
    ;;   This may be repeated as many times as you like.
    (pre-excavated-map scr)
    (let [n-rooms 8] ;; the number of rooms in the dungeon
        (print-screen scr n-rooms))
    (s/redraw scr)

    ;; You must stop the screen before you load this file into a repl.
    ;; If you load a file into the repl while you have a screen open, the open
    ;;   screen will freeze and will not close unless you quit the repl. (only
    ;;   the lanterna screen freezes. Your computer screen should be unaffected
    ;;   You can open a new screen over the frozen screen. The new screen will
    ;;   works with no apparent problems.
    (s/stop scr)
    )
