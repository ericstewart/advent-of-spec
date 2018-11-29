(ns aos.instrument
  (:require [speculative.instrument :refer [instrument]]
            #?(:cljs [cljs.test :as t :refer [run-tests]]
               :clj [clojure.test :as t :refer [run-tests]])))

#?(:cljs
   (do
     (defmethod cljs.test/report [:cljs.test/default :begin-test-var] [m]
       ;; for debugging:
       (println ":begin-test-var" (cljs.test/testing-vars-str m))
       )
     (defmethod cljs.test/report [:cljs.test/default :end-test-var] [m]
       ;; for debugging:
       (println ":end-test-var" (cljs.test/successful? m))
       )))

(println "Instrumenting...")
(println (instrument))
