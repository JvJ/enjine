(ns enjine.core.EnjineDebug
  (:use [penumbra.opengl])
  (:import (org.jbox2d.common IViewportTransform
                              Transform
                              Vec2
                              Color3f)
           (org.jbox2d.collision AABB)
           (org.jbox2d.callbacks DebugDraw
                                 ))
  (:gen-class
   :extends org.jbox2d.callbacks.DebugDraw
   :state state
   :init init
   :constructors {[org.jbox2d.common.IViewportTransform] [org.jbox2d.common.IViewportTransform]   
                  }))

(defn -init
  [ivpt]
  [[ivpt] (atom
           {:vp-trans ivpt})])

(def ^:dynamic *circle-verts* 20)

(defn circle-draw
  "Draws a circle."
  [[x y] radius resolution clr filled?]
  (let [angle (/ (* 2 Math/PI) resolution)
        verts (map (fn [n]
                     (let [an (* n angle)
                           cos (Math/cos an)
                           sin (Math/sin an)]
                       [(+ x (* cos radius))
                        (+ y (* sin radius))]))
                   (range resolution))]
    (if filled?
      (draw-polygon
       (color clr)
       (doseq [[xx yy] verts]
         (vertex xx yy)))
      (draw-lines
       (color clr)
       (doseq [[xx yy] verts]
         (vertex xx yy))))))
  
(defn -drawCircle
  ^void [this ^Vec2 center ^float radius ^Color3f clr]
  (println "drawCircle is taking place!")
  (circle-draw [(.x center) (.y center)]
               radius
               [(.x clr) (.y clr) (.z clr)]
               false))

(defn -drawSolidCircle
  ^void [this ^Vec2 center ^float radius ^Color3f clr]
  (println "drawSolidCircle is taking place!")
  (circle-draw [(.x center) (.y center)]
               radius
               [(.x clr) (.y clr) (.z clr)]
               true))

(defn -drawPoint
  ^void [this ^Vec2 point ^float radius-on-screen ^Color3f clr]
  (println "drawSolidPolygon is taking place!")
  (circle-draw [(.x point) (.y point)]
               radius-on-screen
               [(.x clr) (.y clr) (.z clr)]
               true))

(defn -drawSegment
  ^void [this ^Vec2 p1 ^Vec2 p2 ^Color3f clr]
  (println "drawSegment is taking place!")
  (draw-lines
   (color (.x clr) (.y clr) (.z clr))
   (vertex (.x p1) (.y p1))
   (vertex (.x p2) (.y p2))))

(defn -drawAABB
  ^void [this ^AABB aabb ^Color3f clr]
  (println "drawAABB is taking place!")
  (let [bl (.lowerBound aabb)
        tr (.upperBound aabb)
        [l b] [(.x bl) (.y bl)]
        [r t] [(.x tr) (.y tr)]]
    (draw-line-loop
     (color clr)
     (vertex l b)
     (vertex r b)
     (vertex r t)
     (vertex l t))))

(defn -drawSolidPolygon
  ^void [this verts ^int v-count ^Color3f clr]
  (println "drawSolidPolygon is taking place!")
  (draw-polygon
   (color clr)
   (let [vs (vec verts)]
     (doseq [r (range v-count)]
       (vertex (vs r))))))

(defn -drawTransform
  ^void [this ^Transform trans]
  (println "drawTransform is taking place!")
  ;;(str "DebugDraw with transform: " (@(.state this) :vp-trans) )
  )

(defn -toString
  [this]
  (str
   "Debug draw: \n" @(.state this)))