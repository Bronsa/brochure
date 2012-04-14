(ns brochure.lang.PersistentVector)

(deftype PersistentVector []

  ;; ISeqable
  ;; (-seq [this]
  ;;   (if (pos? (-count this))
  ;;     (->Seq this 0)))
  
  IFn
  (-invoke [this idx]
    (anssert (integer? idx) "Key Must be integer")
    (-nth this (.intValue idx))))
