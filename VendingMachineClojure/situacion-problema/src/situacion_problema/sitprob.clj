(ns situacion-problema.sitprob
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn get-file-lines [filename]
  (with-open [rdr (io/reader (io/resource filename))]
    (reduce conj [] (line-seq rdr))))

(def money (read-string (first (get-file-lines "machine.txt"))))
(def slots (read-string (second (get-file-lines "machine.txt"))))
(def transacciones (read-string (first (get-file-lines "transaccionesfull.txt"))))

(defn check_coins_limit [trans money]
  (if (nil? money) false
      (if (= (first (first money)) (first trans))
        (if (>= (+ (first (rest (first money))) (first (rest trans))) (first (rest (rest (first money))))) false
            true)
        (check_coins_limit trans (rest money)))))

(defn contains [lista x]
  (cond
    (nil? lista) false
    (= (first (first lista)) x) true
    :else (contains (rest lista) x)))

(defn enough_inv? [prod lista]
  (if (nil? lista) false
      (if (= (first (first lista)) prod)
        (if (>= (first (rest (rest (first lista)))) 1) true
            false)
        (enough_inv? prod (rest lista)))))

(defn is_sequence_enough? [sequence start target]
  (if (nil? sequence) false
      (if (>= (+ (first sequence) start) target) true
          (is_sequence_enough? (rest sequence) (+ (first sequence) start) target))))

(defn enough_money? [trans slots]
  (if (nil? slots) false
      (if (= (first (first slots)) (first trans))
        (if (is_sequence_enough? (first (rest trans)) 0
                                 (first (rest (first slots)))) true false)
        (enough_money? trans (rest slots)))))

(defn known-coin? [coin list]
  (if (nil? list) false
      (if (= coin (first list)) true
          (known-coin? coin (rest list)))))

(defn all-true? [lst]
  (or (nil? lst) (and (= (first lst) true)
                      (all-true? (rest lst)))))


(defn update-inv [prod slots]
  (if (nil? slots) '()
      (if (= (first (first slots)) prod) (concat (list (cons (first (first slots))
                                                             (cons (first (rest (first slots)))
                                                                   (cons (- (first (rest (rest (first slots)))) 1) nil))))
                                                 (update-inv prod (rest slots)))
          (concat (list (cons (first (first slots)) (cons (first (rest (first slots)))
                                                          (cons (first (rest (rest (first slots)))) nil))))
                  (update-inv prod (rest slots))))))
