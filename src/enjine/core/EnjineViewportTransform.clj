(ns enjine.core.EnjineViewportTransform
  (:import (org.jbox2d.common IViewportTransform
                              Vec2))
  (:gen-class
   :implements [org.jbox2d.common.IViewportTransform]
   :state state
   :init init
   :constructors {[] []
                  [org.jbox2d.common.Vec2 org.jbox2d.common.Vec2 Float] [] ;TODO: Find some appropriate ctor params?
                  }))

(defn -init
  ([] (-init (Vec2. 0 0) (Vec2. 0 0) 1))
  ([^Vec2 extents ^Vec2 center ^float ratio]
     [[] (atom {:extents extents
                :orig-extents extents
                :center center
                :ptm-ratio ratio})]))

(defn -setCenter ^void
  ([this ^Vec2 v] (.setCenter this (.x v) (.y v)))
  ([this ^float x, ^float y]
     (swap! (.state this) assoc :center (Vec2. x y))))

(defn -setCamera
  ^void [this ^float x ^float y ^float scale]
  (swap! (.state this) assoc :extents (.mul
                                       (@(.state this) :orig-extents)
                                       scale))
  (swap! (.state this) assoc :center (Vec2. x y)))

(defn -getExtents
  ^Vec2 [this]
  (:extents @(.state this)))

(defn -getCenter
  ^Vec2 [this]
  (:center @(.state this)))

(defn -setExtents
  (^void [this ^Vec2 v] (.setExtents this (.x v) (.y v)))
  (^void [this ^float half-width ^float half-height]
         ;; Gotta set the attribute
         (swap! (.state this) :extents [half-width half-height])))

(defn -setYFlip ^void
  [this ^boolean b]
  (swap! (.state this) assoc :yflip b))

(defn -isYFlip
  ^boolean [this]
  (if (@(.state this) :yfilp) true false))

(defn -getWorldToScreen
  ^void [this ^Vec2 v1 ^Vec2 v2])

(defn -getScreenToWorld
  ^void [this ^Vec2 v1 ^Vec2 v2])

(defn -getScreenVectorToWorld
  ^void [this ^Vec2 v1 ^Vec2 v2])

(defn -getWorldVectorToScreen
  ^void [this ^Vec2 v1 ^Vec2 v2])

(defn -toString
  [this]
  (str "VPT: \n" @(.state this)))