(ns enjine.core
  (:use (penumbra opengl)
        (cljutils core))
  (:require [penumbra.app :as app]
            [penumbra.data :as data])
  (:import (org.jbox2d.dynamics World
                                BodyDef
                                Body
                                BodyType
                                Fixture
                                FixtureDef)
           (org.jbox2d.common Vec2
                              Vec3)
           (org.jbox2d.callbacks DebugDraw)
           (org.jbox2d.testbed.framework.j2d DebugDrawJ2D)
           (org.jbox2d.collision.shapes PolygonShape)
           (enjine.core EnjineDebug
                        EnjineViewportTransform)))


(def ^World ^:dynamic *phys-world*
  (atom nil))

(def ^DebugDraw ^:dynamic *dbg-draw*
  (atom nil))


(defn make-phys-box
  []
  (let [sd (PolygonShape.)
        bd (BodyDef.)]
    (set! (.type bd) BodyType/DYNAMIC)
    (.setAsBox sd (float 50) (float 50))
    (println "The poly shape: " sd)
    (.set (.position bd) (float 300) (float 300))
    (let [body (.createBody @*phys-world* bd)
          fix (FixtureDef.)]
      (set! (.shape fix) sd)
      (set! (.density fix) 1)
      (.createFixture body fix))))
    
  


(defn init-physics
  [state]

  ;; Initialize the world
  (println "Starting physics init")
  
  (let [[v b] (:gravity state)]
    (reset! *phys-world* (World. v b)))

  (println "We got the phys world: " @*phys-world*)
  
  (println "Got it")

  (-:>
   (:= [w h] (:dim state))
   (println "Got dims: " [w h])
   (:= evy (EnjineViewportTransform.
            (Vec2. (/ w 2) (/ h 2))
            (Vec2. (/ w 2) (/ h 2))
            (float 1)))
   (:= edb (EnjineDebug. evt))
   (reset! *dbg-draw* edb)
   (.setFlags edb (reduce bit-or [DebugDraw/e_shapeBit
                                  DebugDraw/e_jointBit]))
   (.setDebugDraw @*phys-world* edb)
   (println "debug draw set!"))
  
  ;;(let [[w h] (:dim state)]
  ;;  (println "Got dims: " [w h])
    ;; (let [evt (EnjineViewportTransform.
    ;;            (Vec2. (/ w 2) (/ h 2))
    ;;            (Vec2. (/ w 2) (/ h 2))
    ;;            (float 1))]
    ;;   (println "Evt initialized")
    ;;   (let [edb (EnjineDebug. evt)]
    ;;     (println "EDB initialized")
    ;;     (reset! *dbg-draw* edb)
    ;;     (.setFlags edb (reduce bit-or
    ;;                            [DebugDraw/e_shapeBit
    ;;                             DebugDraw/e_jointBit]))
    ;;     (.setDebugDraw @*phys-world* edb)
    ;;     (println "debug draw set"))))

  ;; Set debug draw, maybe
  ;;(when (state :debug-draw)
  ;;  (let [[w h] (:dim state)]
  ;;    (.setDebugDraw *phys-world*
  ;;                  (EnjineDebug.
  ;;                   (EnjineViewportTransform.
  ;;                     (Vec2. (/ w 2) (/ h 2))
  ;;                     (Vec2. (/ w 2) (/ h 2))
  ;;                     1)))))
  ;;LEFTOFF: HERE!
  (println "Mark 1")

  (make-phys-box)
  
  state)

(defn update
  [[delta time] state]
  (try
    (do
      (.step @*phys-world* delta 50 50)
      ;;(println (.getFlags @*dbg-draw*)))
      (println (.state @*dbg-draw*))
      )
    (catch Exception e
      (println "Exception " e)))
  state)

(defn init
  "State initialization."
  [state]
  
  (apply app/display-mode! (:dim state))
  
  (let [st (init-physics state)]
    (app/title!(state :title))
    (app/periodic-update! 10 (fn [&x] (app/frequency! 10)))
    st))

(defn reshape
  [[x y w h] state]
  (ortho-view 0 w 0 h -1 1)
  (println x ", " y ", " w ", " h)
  (assoc state :dim [w h]))

(def circle-draw)
(defn display [[delta time] state]
  (clear)
;;  (draw-triangles
;;   (color 1 1 1)
;;   (vertex 0 0 0)
;;   (vertex 100 0 0)
  ;;   (vertex 0 100 0))
  (circle-draw [100 100] 50 20)
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

(defn start-window
  []
  
  (app/start

   ;; Callbacks
   {:init init
    :reshape reshape
    :update update
    :display display
    }
   
   ;; State
   {:title "Fhtagn"
    :debug-draw true
    :gravity [(Vec2. 0 0) false]
    :dim [1023 600]
    }
   ))

