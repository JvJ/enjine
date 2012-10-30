(ns enjine.architecture.stateclass)

(def- *hierarchies*
  "Hierarchies associated with each class.  States are represented as keywords."
  (atom {}))

(defprotocol StateClass
  (state [] "Get current state.")
  (trans [st] "Transition to new state."))


(defmacro defstateclass
  [name [& fields] & states+methods]
  (let [(->>
         (partition-by keyword?)
         ;;TODO: This later..?
        ]