(ns rule-engine.core)

(defrecord Transition [predicate next-step])
(defrecord Step [name transitions required-values])

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

(def root-step (->Step "Root"
                       [(->Transition #(> (:age @patient) 42)
                                      (map->Step {:name "Is old"}))
                        (->Transition (constantly true)
                                      (map->Step {:name "Is young"}))]
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