(ns enjine.core.input
  ;;(:use (penumbra opengl))
  (:require [penumbra.app :as app]))

(def ^:dynamic *keys-held*
  (atom {}))

(def ^:dynamic *keys-pressed*
  (atom #{}))

(def ^:dynamic *keys-released*
  (atom #{}))

(defn key-press
  [key state]
  (swap! *keys-pressed* conj key)
  (swap! *keys-held* assoc key 0)
  state)

(defn key-release
  [key state]
  (swap! *keys-released* conj key)
  (swap! *keys-held* dissoc key)
  state)

(defn key-update
  [[delta time]]
  (letfn [(update-map
            [m]
            (into m (for [[k v] m]
                      [k (+ v delta)])))]
    (swap! *keys-held* update-map)))

(defn key-update-end
  [[delta time]]
  (reset! *keys-pressed* #{})
  (reset! *keys-released* #{}))
  
(defn key-reset
  []
  (reset! *keys-held* {})
  (reset! *keys-pressed* #{})
  (reset! *keys-released* #{}))