(ns dev
  (:require [clj-time.core :as time]
            [clj-time.format :as format]
            [clojure.test :as clj-test]))

(println "dot-clojure dev")

(defonce ^:private tap-history (atom '()))

(defn <ex []
  *e)

(add-tap
  (fn [tap-value]
    (when (> (count @tap-history) 9)
      (swap! tap-history drop-last))
    (swap! tap-history (partial cons {:time  (format/unparse (:date-time format/formatters) (time/now))
                                      :value tap-value}))))

(defn >tap-reset []
  (reset! tap-history '()))

(defn >tap [v]
  (tap> v))

(defn <tapn
  ([] @tap-history)
  ([n] (take n @tap-history)))

(defn <tap []
  (first (<tapn)))

(defn run-test
  ([test] (run-test test >tap-reset (comp reverse <tapn)))
  ([test fn-before fn-after]
   (fn-before)
   (clj-test/test-vars [test])
   (fn-after))
  )

(defn map-debug
  ([arg] (map-debug println arg))
  ([debug-fn arg]
   (debug-fn arg)
   arg))

(defn fn-debug
  ([fn-name fn] (fn-debug fn-name fn >tap))
  ([fn-name fn debug-fn]
   (let [res (fn)]
     (debug-fn {fn-name res}))))

(comment
  (>tap {:hello "world"})
  (<tapn)
  (>tap-reset)

  (fn-debug :sum #(sum [1 2 3]))
  (run-test #(sum [1 2 3]))

  (defn sum [coll]
    (:res (reduce
            (fn [acc n]
              (>tap {:acc acc})
              (-> acc
                  (update :idx inc)
                  (update :res (partial + n))))
            {:idx 0 :res 0}
            coll))
    )

  (do
    (>tap-reset)
    (sum [1])
    (<tapn)
    )

  )