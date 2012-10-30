(ns triangleman
  (:import (org.jbox2d.dynamics World
                                BodyDef
                                Body
                                BodyType
                                Fixture
                                FixtureDef))
  (:use [enjine.core]
        [enjine.core.input]))


(defn makeTriangleMan
  [identifier x y scale]
  (let [forward [(* scale 5000) 0]
        turn (* scale 5000); In radians
        verts [[1 0] [-2 1] [-2 -1]]
        s-verts (map (comp vec (partial map #(* scale %))) verts)
        pb (add-poly-body :pos [x y]
                          :vel [0 0]
                          :dens 1
                          :verts s-verts)
        _ (.setAngularDamping pb 1.0)
        ]
    {identifier
     {:phys-body pb
      :forward forward
      :turn turn
      :draws [
              (fn [m]
                (penumbra.text/write-to-screen (str (:forward m)) 0 30)
                m
                )
              ]
      :updates [
                (fn [m]
                  (cond
                   (@*keys-held* "w")
                   (do
                     (.applyLinearImpulse (:phys-body m)
                                          (.getWorldVector (:phys-body m) (v-b2v (:forward m)))
                                          (.getWorldPoint (:phys-body m) (v-b2v [0 0])))))
                   (cond
                    (@*keys-held* "a")
                    (do
                      (.applyAngularImpulse (:phys-body m) (float (:turn m))))

                    (@*keys-held* "d")
                    (do
                      (.applyAngularImpulse (:phys-body m) (- (float (:turn m))))))
                  (let [newV (cond
                              (@*keys-held* "=") (v+ (:forward m) [(* 100 scale) 0])
                              (@*keys-held* "-") (v- (:forward m) [(* 100 scale) 0])
                              :else (:forward m))
                        newT (cond
                              (@*keys-held* "[") (+ (:turn m) (* 100 scale))
                              (@*keys-held* "]") (- (:turn m) (* 100 scale))
                              :else (:turn m))]
                    (assoc m :turn newT :forward newV)
                    ))
                ]
     }}))