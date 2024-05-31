(ns cljd.storm.emitter
  (:require [clojure.string :as str]))

#?(:clj (def instrument-enable
          (some-> (System/getProperty "cljs.storm.instrumentEnable")
                  Boolean/parseBoolean)))

#?(:clj (def instrument-only-prefixes
          (some-> (System/getProperty "cljs.storm.instrumentOnlyPrefixes")
                  (str/split #","))))

#?(:clj (def instrument-skip-prefixes
          (some-> (System/getProperty "cljs.storm.instrumentSkipPrefixes")
                  (str/split #","))))

#?(:clj (def instrument-skip-regex
          (when-let [re (System/getProperty "cljs.storm.instrumentSkipRegex")]
            (re-pattern re))))

#?(:clj (defn set-instrumentation [on?]
          (alter-var-root #'instrument-enable (constantly on?))))

#?(:clj (defn add-instrumentation-only-prefix [p]
          (alter-var-root #'instrument-only-prefixes conj p)))

#?(:clj (defn remove-instrumentation-only-prefix [p]
          (alter-var-root #'instrument-only-prefixes (fn [iop] (remove #(= % p) iop)))))

#?(:clj (defn add-instrumentation-skip-prefix [p]
          (alter-var-root #'instrument-skip-prefixes conj p)))

#?(:clj (defn remove-instrumentation-skip-prefix [p]
          (alter-var-root #'instrument-skip-prefixes (fn [isp] (remove #(= % p) isp)))))

#?(:clj (defn set-instrumentation-skip-regex [re]
          (alter-var-root #'instrument-skip-regex (constantly (re-pattern re)))))

#?(:clj (defn remove-instrumentation-skip-regex []
          (alter-var-root #'instrument-skip-regex (constantly nil))))

#?(:clj (defn skip-instrumentation? [ns-symb]
          (let [nsname (str ns-symb)
                instrument? false
                instrument? (reduce (fn [inst? p]
                                      (or inst? (str/starts-with? nsname p)))
                                    instrument?
                                    instrument-only-prefixes)
                instrument? (reduce (fn [inst? p]
                                      (and inst? (not (str/starts-with? nsname p))))
                                    instrument?
                                    instrument-skip-prefixes)
                instrument? (if-not instrument-skip-regex
                              instrument?
                              (and instrument? (not (re-find instrument-skip-regex nsname))))]
            ;; skip iff
            (or (not instrument-enable)
                (not instrument?)))))

(defn maybe-instrument-wrap-expr [dart-x x coord ns form-id]
  (if (and (not (= form-id -480351059))
           coord
           (not (skip-instrumentation? ns)))
    (let [str-coord (str/join "," coord)
          sym (gensym "storm_expr_")]
      `(dart/let
           ([~sym ~dart-x])
         (lcocs_tracer.trace_expr ~sym ~str-coord ~form-id)
         sym#))
    dart-x))
