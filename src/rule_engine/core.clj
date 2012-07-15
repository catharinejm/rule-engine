(ns rule-engine.core)

(defmulti make-step
  (fn [x & _] (type x)))
(defmethod make-step :step
  [step] step)
(defmethod make-step String
  [name & [transitions required-values]]
  (make-step {:name name
              :transitions transitions
              :required-values required-values}))
(defmethod make-step clojure.lang.IPersistentMap
  [smap]
  (with-meta
    (merge {:name nil :transitions nil :required-values nil} smap)
    {:type :step}))

(defmulti make-transition
  (fn [x & _] (type x)))
(prefer-method make-transition clojure.lang.IPersistentMap clojure.lang.IFn)
(defmethod make-transition :transition
  [transition] transition)
(defmethod make-transition clojure.lang.IPersistentMap
  [tmap]
  (with-meta
    (merge {:prediate nil :next-step nil} tmap)
    {:type :transition}))
(defmethod make-transition clojure.lang.IFn
  [predicate next-step]
  (make-transition {:predicate predicate
                    :next-step next-step}))

(def patient (atom {}))
(def coercers
  (letfn [(to-num [n]
            (try (Long/parseLong n)
                 (catch NumberFormatException e
                   nil)))]
    {:age to-num}))

(defn update-patient! [new-map]
  (let [coerce-fn (fn [k] (get coercers k identity))
        coerced-map (reduce (fn [map [k v]]
                              (assoc map k ((coerce-fn k) v)))
                            {} new-map)]
    (swap! patient #(merge % coerced-map))))

(def root-step (make-step "Root"
                          [(make-transition #(> (:age @patient) 42)
                                            (make-step {:name "Is old"}))
                           (make-transition (constantly true)
                                            (make-step {:name "Is young"}))]
                          #{:age}))

(defn children [step]
  (map :next-step (:transitions step)))

(defn final? [step] (empty? (:transitions step)))

(defn missing-attributes [step]
  (let [ptnt-map @patient]
    (filter #(nil? (ptnt-map %)) (:required-values step))))

(defn ready-to-transition? [step]
  (empty? (missing-attributes step)))

(defn next-step [step]
  (let [missing (missing-attributes step)]
    (if (empty? missing)
      (some #(when ((:predicate %)) (:next-step %))
            (:transitions step))
      (assoc step :missing-attributes missing))))

(defn get-attributes! [attrs]
  (update-patient! (reduce
                    (fn [map key]
                      (println "Enter" (name key))
                      (assoc map key (read-line)))
                    {} attrs)))

(defn run!
  ([] (run! root-step))
  ([step]
     (loop [step step]
       (if (final? step)
         step
         (let [nxt (next-step step)]
           (when (contains? nxt :missing-attributes)
             (get-attributes! (:missing-attributes nxt)))
           (recur nxt))))))

(defn -main [] (run!))