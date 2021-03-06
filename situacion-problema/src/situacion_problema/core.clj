(ns situacion-problema.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn get-file-lines [filename]
  (with-open [rdr (io/reader (io/resource filename))]
    (reduce conj [] (line-seq rdr))))

(def money1 (read-string (first (get-file-lines "machine.txt"))))
(def slots1 (read-string (second (get-file-lines "machine.txt"))))
(def transacciones1 (read-string (first (get-file-lines "transaccionesfull.txt"))))
(def machineid1 "machine1")

(defn check_coins_limit [trans money]
  (if (empty? money) false
      (if (= (first (first money)) (first trans))
        (if (>= (+ (first (rest (first money))) (first (rest trans))) (first (rest (rest (first money))))) false
            true)
        (check_coins_limit trans (rest money)))))

(defn contains [lista x]
  (cond
    (empty? lista) false
    (= (first (first lista)) x) true
    :else (contains (rest lista) x)))

(defn enough_inv? [prod lista]
  (if (empty? lista) false
      (if (= (first (first lista)) prod)
        (if (>= (first (rest (rest (first lista)))) 1) true
            false)
        (enough_inv? prod (rest lista)))))

(defn is_sequence_enough? [sequence start target]
  (if (empty? sequence) false
      (if (>= (+ (first sequence) start) target) true
          (is_sequence_enough? (rest sequence) (+ (first sequence) start) target))))

(defn enough_money? [trans slots]
  (if (empty? slots) false
      (if (= (first (first slots)) (first trans))
        (if (is_sequence_enough? (first (rest trans)) 0
                                 (first (rest (first slots)))) true false)
        (enough_money? trans (rest slots)))))

(defn known-coin? [coin list]
  (if (empty? list) false
      (if (= coin (first list)) true
          (known-coin? coin (rest list)))))

(defn all-true? [lst]
  (every? true? lst))


(defn update-inv [prod slots]
  (if (empty? slots) '()
      (if (= (first (first slots)) prod) (concat (list (cons (first (first slots))
                                                             (cons (first (rest (first slots)))
                                                                   (cons (- (first (rest (rest (first slots)))) 1) nil))))
                                                 (update-inv prod (rest slots)))
          (concat (list (cons (first (first slots)) (cons (first (rest (first slots)))
                                                          (cons (first (rest (rest (first slots)))) nil))))
                  (update-inv prod (rest slots))))))


(defn update-money [seq money]
  (if (empty? money) '()
      (if (= (first seq) (first (first money))) (concat (list (cons (first (first money))
                                                                    (cons (+ (first (rest (first money))) (first (rest seq))) nil)))
                                                        (update-money seq (rest money)))
          (concat (list (cons (first (first money)) (cons (first (rest (first money))) nil)))
                  (update-money seq (rest money))))))


(defn update-money-minus [seq money]
  (if (empty? money) '()
      (if (= (first seq) (first (first money))) (concat (list (cons (first (first money))
                                                                    (cons (- (first (rest (first money))) (first (rest seq))) nil)))
                                                        (update-money seq (rest money)))
          (concat (list (cons (first (first money)) (cons (first (rest (first money))) nil)))
                  (update-money-minus seq (rest money))))))


(defn elimina-ocurrencias [value lista]
  (if (empty? lista) '()
      (if (= value (first lista)) (elimina-ocurrencias value (rest lista))
          (cons (first lista) (elimina-ocurrencias value (rest lista))))))

(defn counter [value lst]
  (if (empty? lst) 0
      (if (= value (first lst)) (+ 1 (counter value (rest lst)))
          (counter value (rest lst)))))

(defn encode [lista]
  (if (empty? lista) '()
      (cons
       (cons (first lista) (cons (counter (first lista) lista) nil)) (encode (elimina-ocurrencias (first lista) lista)))))

(defn last_element [lista]
  (last lista))

(defn repeat-money [seq money]
  (if (empty? seq) '()
      (filter (fn [x] (if (= x '()) false true))
              (cons (update-money (first seq) money)
                    (repeat-money (rest seq) (update-money (first seq) money))))))

(defn repeat-money-down [seq money]
  (if (empty? seq) '()
      (filter (fn [x] (if (= x '()) false true))
              (cons (update-money-minus (first seq) money)
                    (repeat-money-down (rest seq) (update-money (first seq) money))))))

(defn final-money [x]
  (last x))

(defn change [value money]
  (if (empty? money) '()
      (if (= (quot value (first (first money))) 0) (change value (rest money))
          (if (and (= (quot value (first (first money))) value) (>= (first (rest (first money))) (quot value (first (first money)))))
            (cons (list (first (first money)) (quot value (first (first money))))
                  (change value (rest money)))
            (if (and (>= (quot value (first (first money))) 1) (>= (first (rest (first money))) (quot value (first (first money)))))
              (cons (list (first (first money)) (quot value (first (first money))))
                    (change (rem value (first (first money))) (rest money)))
              (change value (rest money)))))))

(defn amount_change [trans slots]
  (if (empty? slots) 0
      (if (= (first trans) (first (first slots))) (- (apply + (first (rest trans))) (second (first slots)))
          (amount_change trans (rest slots)))))

(defn sum-change [change]
  (if (empty? change) 0
      (+ (apply * (first change)) (sum-change (rest change)))))

(defn enough-change? [trans slots money]
  (cond
    (= (amount_change trans slots) 0) (list "No change needed!")
    (empty? (change (amount_change trans slots) (reverse money))) (quote "No available change")
    (not (= (sum-change (change (amount_change trans slots) (reverse money))) (amount_change trans slots))) (quote "Not enough change available")
    (not (pos-int? (sum-change (change (amount_change trans slots) (reverse money))))) (quote "Error in calculation")
    :else (change (amount_change trans slots) (reverse money))))


(defn check-transaction [trans slots monedas generalMoney]
  (if (= (contains slots (first trans)) false) (quote "Unvalid product (Does not exist)")
      (if (= (enough_inv? (first trans) slots) false) (quote "No inventory")
          (if (= (enough_money? trans slots) false) (quote "Not enough money introduced")
              (if (= (all-true? (map (fn [x] (known-coin? x (map first monedas))) (first (rest trans)))) false) (quote "Unknown coin")
                  (if (= (all-true? (map (fn [x] (check_coins_limit x generalMoney)) (encode (first (rest trans))))) false) (quote "Ups, coin limit reached")
                      (if (and (not (list? (enough-change? trans slots monedas))) (not (= (type (enough-change? trans slots monedas)) clojure.lang.Cons))) (enough-change? trans slots monedas)
                          (if (and (enough_inv? (first trans) slots) (enough_money? trans slots)) true
                              false))))))))

(defn validated [slots monedas transacciones]
  (list "Transaction Successful!" (update-inv (first (first transacciones)) slots)
        (final-money (repeat-money (encode (first (rest (first transacciones)))) monedas))
        (if (list? (first (enough-change? (first transacciones) slots monedas)))
          (list "Change: " (enough-change? (first transacciones) slots monedas)
                "Updated money: " (final-money (repeat-money-down
                                                (enough-change? (first transacciones) slots monedas)
                                                (final-money (repeat-money (encode (first (rest (first transacciones)))) monedas)))))
          (enough-change? (first transacciones) slots monedas))))

(defn validate_transaction [slots monedas transacciones generalMoney]
  (if (empty? transacciones) '()
      (if (= (check-transaction (first transacciones) slots monedas generalMoney) true) (validated slots monedas transacciones)
          (check-transaction (first transacciones) slots monedas generalMoney))))


(defn checks_all_transactions [slots monedas transacciones generalMoney]
  (if (empty? transacciones) '()
      (if (list? (validate_transaction slots monedas transacciones generalMoney))
        (if (= (first (last_element (validate_transaction slots monedas transacciones generalMoney))) "No change needed!")
          (cons (validate_transaction slots monedas transacciones generalMoney)
                (checks_all_transactions (first (rest (validate_transaction slots monedas transacciones generalMoney)))
                                         (first (rest (rest (validate_transaction slots monedas transacciones generalMoney))))
                                         (rest transacciones)
                                         generalMoney))
          (cons (validate_transaction slots monedas transacciones generalMoney)
                (checks_all_transactions (first (rest (validate_transaction slots monedas transacciones generalMoney)))
                                         (last_element (last_element (validate_transaction slots monedas transacciones generalMoney)))
                                         (rest transacciones)
                                         generalMoney)))
        (cons (list (validate_transaction slots monedas transacciones generalMoney)) (checks_all_transactions slots monedas (rest transacciones) generalMoney)))))


; Results Section 

(defn imbricada? [lista] (if (empty? lista) false (if (list? (first lista)) true (imbricada? (rest lista)))))

(defn get-last-valid-transaction [results]
  (if (empty? results) '()
      (if (imbricada? (first results)) (first results)
          (get-last-valid-transaction (rest results)))))

(defn get-updated-money [lastvalidtrans]
  (if (= (first (last_element lastvalidtrans)) "No change needed!") (first (rest (rest lastvalidtrans)))
      (last_element (last_element lastvalidtrans))))


(defn overallwins [new old]
  (cond
    (empty? new) '()
    (empty? old) '()
    :else (cons (- (first (rest (first new))) (first (rest (first old)))) (overallwins (rest new) (rest old)))))

(defn low-inv [slots]
  (if (empty? slots) '()
      (if (or (and (<= (last_element (first slots)) 5) (>= (last_element (first slots)) 1)) (zero? (last_element (first slots))))
        (cons (list "Product: " (first (first slots)) "Q: " (last_element (first slots))) (low-inv (rest slots)))
        (low-inv (rest slots)))))

(defn fullcoins? [new old]
  (cond
    (empty? new) '()
    (empty? old) '()
    :else (if (or (and (< (first (rest (first new))) (last_element (first old)))
                       (>= (first (rest (first new))) (- (last_element (first old)) 10)))
                  (= (first (rest (first new))) (last_element (first old))))
            (cons (list "Coin: " (first (first new)) "Inv: " (first (rest (first new))) "Limit" (last_element (first old))) (fullcoins? (rest new) (rest old)))
            (fullcoins? (rest new) (rest old)))))

(defn emptycoins? [new]
  (cond
    (empty? new) '()
    :else (if (or (and (<= (first (rest (first new))) 10)
                       (>= (first (rest (first new))) 1))
                  (zero? (first (rest (first new)))))
            (cons (list "Coin: " (first (first new)) "Inv: " (first (rest (first new)))) (emptycoins? (rest new)))
            (emptycoins? (rest new)))))


(defn review [newmoney oldmoney newslots generalMoney]
  (list
   "<------- Earnings ------->" (if (zero? (apply + (map (fn [x] (if (pos-int? x) x 0)) (overallwins newmoney generalMoney)))) "No earnings"
                                    (apply + (map (fn [x] (if (pos-int? x) x 0)) (overallwins newmoney generalMoney))))
   "<------- Inventory ------->" (if (empty? (low-inv newslots)) "No maintenance in inventory needed" (low-inv newslots))
   "<------- Full Coins ------->" (if (empty? (fullcoins? newmoney oldmoney)) "No full/almost full coins" (fullcoins? newmoney oldmoney))
   "<------- Empty Coins ------->" (if (empty? (emptycoins? newmoney)) "No empty/almost empty coins" (emptycoins? newmoney))))

(defn filter-results [resultados]
  (if (empty? resultados) '()
      (if (imbricada? (first resultados)) (cons (list (first (first resultados)) (if (imbricada? (last_element (first resultados)))
                                                                                   (first (rest (last_element (first resultados))))
                                                                                   (last_element (first resultados))))
                                                (filter-results (rest resultados)))
          (cons (list (first resultados)) (filter-results (rest resultados))))))




(defn processMachine [slots money transacciones originalMoney id]
  (let [res (checks_all_transactions slots money transacciones originalMoney)
        new-money (get-updated-money (get-last-valid-transaction (reverse res)))
        new-slot (first (rest (get-last-valid-transaction (reverse res))))]
    (list "<---------- Transactions ---------->"
          (filter-results res)
          "<<<<<<<<<<<<< Review >>>>>>>>>>>>>>"
          (review new-money money new-slot originalMoney)
          id)))

(def letras ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N"])

(defn generaLetra []
  (symbol (apply str (repeatedly 1 #(rand-nth letras)))))

(defn generaMoneda []
  (rand-nth [1 2 5 10 20 50]))

(defn generaSecuencia [n]
  (apply list (to-array (repeatedly n generaMoneda))))

(defn generaTransaccion []
  (let [numMonedas (rand-nth (range 1 6))]
    (list (generaLetra) (generaSecuencia numMonedas))))

(defn generaListaTrans []
  (let [numTrans (rand-nth (range 1 36))]
    (apply list (to-array (repeatedly numTrans generaTransaccion)))))

(defn generaProductos [lista]
  (let [prod (first lista)]
    (if (empty? lista) '()
        (cons (list (symbol prod) (rand-nth (range 1 40)) (rand-nth (range 1 40))) 
              (generaProductos (rest lista))))))

; Generate a random list of available products
(take 5 (repeatedly #(generaProductos letras)))
; Generate a random list of transactions
(take 5 (repeatedly #(generaListaTrans)))

; money stays the same?
(def alpha-numeric "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(defn generate-id []
  (apply str
         (repeatedly 15
                     #(rand-nth alpha-numeric))))

(defn generaMaquina [money]
  (list (generaProductos letras) money (generaListaTrans) (generate-id)))

(generaMaquina money1)


(defn resultadosParalelos [lista particiones]
  (doall (pmap (fn [y] (doall (map (fn [x] (processMachine (first x) (second x) (nth x 2) (second x) (last x))) y))) (partition-all particiones lista))))

(resultadosParalelos (take 1 (repeatedly #(generaMaquina money1))) 10)


(defn resultadosNoparalelos [lista]
  (doall (map (fn [x] (processMachine (first x) (second x) (nth x 2) (second x) (last x))) lista)))

(resultadosNoparalelos (take 1 (repeatedly #(generaMaquina money1))))


(defn get-winnings [x]
  (second (first (rest (rest (rest x))))))

; <---------- Overall win ---------->

(defn get-overall-wins [x]
  (apply + (map get-winnings x)))

; <---------- Top 10% winnings ---------->

(defn get-top-ten [x]
  (let [num (/ (count x) 10)]
    (take num (reverse (sort-by get-winnings x)))))

(defn get-win-per-machine [sorted]
  (list "ID: "
        (last sorted)
        "Winnings: "
        (get-winnings sorted)))

(defn getMostWins [lista]
  (map get-win-per-machine (get-top-ten lista)))

; <---------- Resurtido inv ---------->

(defn get-needed-refill-inv [lista]
  (if (empty? lista) '()
      (if (not= (first (rest (rest (rest (nth (first lista) 3))))) "No maintenance in inventory needed")
        (cons (first lista) (get-needed-refill-inv (rest lista)))
        (get-needed-refill-inv (rest lista)))))

(defn get-refill-summary-inv [lista]
  (list "ID: "
        (last lista)
        "Maintenance in: "
        (first (rest (rest (rest (nth lista 3)))))))

(defn who-needs-refill-inv [lista]
 (let [m (get-needed-refill-inv lista)]
   (if (empty? m) (quote "No one needs refill on inventory")
       (map get-refill-summary-inv m))))

; <---------- Resurtido monedas ---------->

(defn get-needed-refill-mon [lista]
  (if (empty? lista) '()
      (if (not= (last (nth (first lista) 3)) "No empty/almost empty coins")
        (cons (first lista) (get-needed-refill-mon (rest lista)))
        (get-needed-refill-mon (rest lista)))))

(defn get-refill-summary-mon [lista]
  (list "ID: "
        (last lista)
        "Maintenance in: "
        (last (nth lista 3))))

(defn who-needs-refill-mon [lista]
  (let [m (get-needed-refill-mon lista)]
    (if (empty? m) (quote "No one needs refill on coins")
        (map get-refill-summary-mon m))))


; <---------- Resurtido monedas ---------->

(defn get-needed-takeout-coins [lista]
  (if (empty? lista) '()
      (if (not= (nth (nth (first lista) 3) 5) "No full/almost full coins")
        (cons (first lista) (get-needed-takeout-coins (rest lista)))
        (get-needed-takeout-coins (rest lista)))))

(defn get-takeout-coins-summary [lista]
  (list "ID: "
        (last lista)
        "Coins that need to be taken out (full):"
        (nth (nth lista 3) 5)))

(defn who-needs-takeout-coins [lista]
  (let [m (get-needed-takeout-coins lista)]
    (if (empty? m) (quote "No one needs coins taken out")
        (map get-takeout-coins-summary m))))

(defn getNormalList [x]
  (if (empty? x) '()
      (cons (first (first x)) (getNormalList (rest x)))))

(defn display-results [lista]
  (list
   "<---------- Overall wins ---------->"
   (get-overall-wins lista)
   "<---------- Top 10% winnings ---------->"
   (getMostWins lista)
   "<---------- Machines that need inventory refil ---------->"
   (who-needs-refill-inv lista)
   "<---------- Machines that need coin refill ---------->"
   (who-needs-refill-mon lista)
   "<---------- Machines that have reached coin limit ---------->"
   (who-needs-takeout-coins lista)))

(defn generaPruebas [tipo num particiones]
  (if (= tipo "P") (resultadosParalelos
                    (cons (list slots1
                                money1
                                transacciones1
                                "machine1") (take num (repeatedly #(generaMaquina money1)))) particiones)
      (resultadosNoparalelos (cons (list slots1
                                         money1
                                         transacciones1
                                         "machine1") (take num (repeatedly #(generaMaquina money1)))))))
