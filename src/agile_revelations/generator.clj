(ns agile-revelations.generator
  (:gen-class))

(defn text->words [text]
  (clojure.string/split text #" |\n|:"))

(defn word-transitions [words]
  (partition-all 3 1 words))

(defn file->words [file]
  (text->words (slurp file)))

(def uncle-bob-words (mapcat file->words ["resources/uncle_bob_1.txt"
                                          "resources/uncle_bob_2.txt"
                                          "resources/uncle_bob_3.txt"]))

(def revelation-words (filter #(re-find #"[a-zA-Z]" %)
                              (file->words "resources/revelations.txt")))


(def transitions (partition-all 3 1 (map clojure.string/lower-case
                                         (concat uncle-bob-words
                                                 uncle-bob-words
                                                 uncle-bob-words
                                                 uncle-bob-words
                                                 uncle-bob-words
                                                 uncle-bob-words
                                                 uncle-bob-words
                                                 uncle-bob-words
                                                 uncle-bob-words
                                                 revelation-words))))


(def transition-map (reduce (fn [r t] (merge-with (comp vec concat) r
                                                  (let [[a b c] t]
                                                    {[a b] (if c [c] [])})))
                            {}
                            transitions))


(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]]
        (recur new-prefix chain (conj result suffix))))))

(defn generate-text [entry-point]
  (->> (walk-chain entry-point transition-map entry-point)
       (clojure.string/join " ")))

;; e.g.
;; (generate-text ["and" "saw"])
