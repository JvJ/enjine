(ns enjine.core
  (:use (penumbra opengl)
        (enjine.core input))
  (:require [penumbra.app :as app]
            [penumbra.data :as data]
            [penumbra.text :as text])
  (:import (org.jbox2d.dynamics World
                                BodyDef
                                Body
                                BodyType
                                Fixture
                                FixtureDef)
           (org.jbox2d.common Vec2
                              Vec3
                              Settings)
           (org.jbox2d.callbacks DebugDraw)
           (org.jbox2d.testbed.framework.j2d DebugDrawJ2D)
           (org.jbox2d.collision.shapes PolygonShape)
           (enjine.core EnjineDebug
                        EnjineViewportTransform)))

(def update-rate 1000/60)

(def ^World ^:dynamic *phys-world*
  (atom nil))

(def ^:dynamic *objects*
  ;; "A set of obejcts.  Objects are maps of:
;; :phys-body : the box2d body associated with this object
;; :updates : a vector of update functions that take the object as a parameter."
  (atom {}))

(defn make-phys-box
  []
  (let [sd (PolygonShape.)
        bd (BodyDef.)]
    (set! (.type bd) BodyType/DYNAMIC)
    (.setAsBox sd (float 50) (float 50))
    ;;(println "The poly shape: " sd
    (set! (.position bd) (Vec2. (float 300) (float 300)))
    (let [body (.createBody @*phys-world* bd)
          fix (FixtureDef.)]
      ;;(reset! *test-bod* body)
      (set! (.shape fix) sd)
      (set! (.density fix) 1)
      ;;(.setPosition body (Vec2. (float 300) (float 300)))
      (.createFixture body fix)
      (.setLinearVelocity body (Vec2. (float 10.0) (float 10.0))))))
    

(defn body-print
  "Print out a list of bodies for debugging purposes."
  []
  (loop [b (.getBodyList @*phys-world*)]
    (when b
      (loop [f (.getFixtureList b)]
        (when f
          (let [pos (.getPosition b)
                vs (vec (.getVertices (.getShape f)))]
            ;;(println "Vecs: " vs)
            (draw-line-loop
             (color 1 1 1)
             (doseq [v vs]
               ;;(println "Drawing vertex: "v)
               (vertex (+ (.x pos)(.x v))
                       (+ (.y pos)(.y v))))))
          (recur (.getNext f))))
      (recur (.getNext b)))))
  


(defn init-physics
  [state]

  ;; Initialize the world
  (println "Starting physics init")
  
  (let [[v b] (:gravity state)]
    (reset! *phys-world* (World. v b)))

  (let [[w h] (:dim state)
        _ (println "Got dims: " [w h])
        evt (EnjineViewportTransform.
                 (Vec2. (/ w 2) (/ h 2))
                 (Vec2. (/ w 2) (/ h 2))
                 (float 1))
        edb (EnjineDebug. evt)
        _ (.setFlags edb (reduce bit-or [DebugDraw/e_shapeBit
                                       DebugDraw/e_jointBit]))
        _ (.setDebugDraw @*phys-world* edb)
        _ (println "debug draw set!")])
  
  state)

(defn update
  [[delta time] state]
  
  (.step @*phys-world* delta 100 100)

  (key-update [delta time])

  ;; Most update logic goes here!
  (doseq [[k o] @*objects*]
    (let [newObj (reduce #(%2 %1) o (:updates o))]
      (swap! *objects* assoc k newObj)))

  
  (key-update-end [delta time])
  
  (assoc state
    :delta delta
    :time time))

(defn init
  "State initialization."
  [state]
  
  (apply app/display-mode! (:dim state))
  
  (let [st (init-physics state)]
    (app/title!(state :title))
    (app/periodic-update! update-rate (fn [st]
                               (app/frequency! update-rate)
                               st))
    st)

  state)

(defn reshape
  [[x y w h] state]
  (ortho-view 0 w 0 h -1 1)
  (assoc state :dim [w h]))

(def circle-draw)
(defn display [[delta time] state]
  (clear)
  (.drawDebugData @*phys-world*)
  (text/write-to-screen (str @*keys-held* "\n\n"
                             @*keys-pressed* "\n\n"
                             @*keys-released* "\n\n"
                             ) 0 0)
  
  (doseq [[k o] @*objects*]
    (let [newObj (reduce #(%2 %1) o (:draws o))]
      (swap! *objects* assoc k newObj)))
  
  state)

(defn circle-draw
  [[x y] radius resolution]
  (let [angle (/ (* 2 Math/PI) resolution)
        verts (map (fn [n]
                     (let [an (* n angle)
                           cos (Math/cos an)
                           sin (Math/sin an)]
                       [(+ x (* cos radius))
                        (+ y (* sin radius))]))
                   (range resolution))]
    (draw-polygon
     (color 1 1 1)
     (doseq [[xx yy] verts]       
       (vertex xx yy)))))


(defn keypress-test
  [key state]
  (println "[delta time] : " (map state [:delta :time]))
  state)

(defn start-window
  []
  (.start
   (Thread.
    (fn []
      (app/start
       ;; Callbacks
       {:init init
        :reshape reshape
        :update update
        :display display
        :key-press key-press
        :key-release key-release
        }
       
       ;; State
       {:title "Fhtagn"
        :debug-draw true
        :gravity [(Vec2. 0 0) true]
        :dim [1023 600]
        :time 0
        :delta 0
        }
       )))))

;;;; =======
;;;; Interface to the engine

(defn b2v-v
  "Convert box2d vector to clojure vector."
  [v]
  (cond
   (= (class v) Vec2) [(.x v) (.y v)]
   (= (class v) Vec3) [(.x v) (.y v) (.z v)]
   :else nil))

(defn v-b2v
  [v]
  "Convert clojure vector to box2d vector."
  (cond
   (= (count v) 2) (Vec2. (float (v 0)) (float (v 1)))
   (= (count v) 3) (Vec3. (float (v 0)) (float (v 1)) (float (v 2)))
   :else nil))

(defn to-d
  "Converts radians to degrees."
  [f]
  (* f (/ 180 Math/PI)))

(defn to-r
  "Convert degrees to radians."
  [f]
  (* f (/ Math/PI 180)))

(defn ang-v
  "Create a vector using angle rom the x axis and a magnitude."
  [angle magnitude]
  [(* magnitude (Math/cos angle)) (* magnitude (Math/sin angle))])

(defn v+
  [& vecs]
  (vec (reduce (partial map +) vecs)))

(defn v-
  [& vecs]
  (vec (reduce (partial map -) vecs)))

(defn v*
  [v s]
  (vec (map #(* s %)) v))

(defn v-div
  [v s]
  (vec (map #(/ % s)) v))
  

(defn add-poly-body
  "Create a polygon body and add it to the world."
  [&{:keys [pos vel dens verts]}]
  (let [ps (PolygonShape.)
        vs (map v-b2v
                (take Settings/maxPolygonVertices verts))
        _ (.set ps (into-array Vec2 vs) (count verts))
        bd (BodyDef.)
        _ (set! (.type bd) BodyType/DYNAMIC)
        _ (set! (.position bd) (v-b2v pos))
        fix (FixtureDef.)
        _ (set! (.shape fix) ps)
        _ (set! (.density fix) (float dens))
        body (.createBody @*phys-world* bd)
        _ (.createFixture body fix)
        _ (when vel (.setLinearVelocity body (v-b2v vel)))
        ]
    ;; This is the return value
    body))