(ns kontor-test.core)

;; pricing is a map from :item to a vector of quantity=>price vectors
(def test-pricing {:A [[1 2] [4 7]]
                   :B [[1 12]]
                   :C [[1 1.25] [6 6]]
                   :D [[1 0.15]]})
(def test-items-1 [:A :B :C :D :A :B :A :A])
(def test-items-2 [:C :C :C :C :C :C :C])
(def test-items-3 [:A :B :C :D])


(defn cart-init []
  "Initialize a new cart atom"
  (atom {:pricing {} :contents []}))

(defn set-pricing [cart pricing]
  "Set the pricing structure for cart"
  (assoc-in cart [:pricing] pricing))

(defn scan [cart item]
  "Scan item or sequence of items into cart"
  (if (vector? item)
    (assoc-in cart [:contents] (vec (concat (:contents cart) item)))
    (assoc-in cart [:contents] (conj (:contents cart) item))))

(defn calc-item [count pricing & [subtotal]]
  "Calculate the total of a single item given the item count,
   item pricing vector [quantity cost] and optional subtotal.
   Recursively calls itself decrementing count by # of items
   matched."
  (let [cur-price (first (filter #(>= count (first %)) pricing))
        cur-total (+ (if (nil? subtotal) 0 subtotal) (second cur-price))
        cur-count (- count (first cur-price))]
    (if (> cur-count 0)
      (calc-item cur-count pricing cur-total)
      cur-total)))

(defn calculate-total [cart]
  "Calculate the total dollar value of items in cart"
  (let [freq (frequencies (:contents cart))
        sorted-prices (into {} (for [[k v] (:pricing cart)] [k (#(reverse (sort-by first %)) v)]))
        keys (keys freq)
        item-totals (map #(vector % (calc-item (% freq) (% sorted-prices))) keys)
        total (reduce + (map #(second %) item-totals))]
    total))


(defn my-test [a b]
  "Simple function to test two results"
  (if (= a b)
    (println (str "Test PASSED: " a "==" b))
    (println (str "Test FAILED: " a "!=" b))))

(defn -main []
  (let [cart-1 (cart-init)
        cart-2 (cart-init)
        cart-3 (cart-init)]
    ;; test 1
    (do
      (reset! cart-1 (set-pricing @cart-1 test-pricing))
      (reset! cart-1 (scan @cart-1 test-items-1))
      (my-test (calculate-total @cart-1) 32.40))

    ;; test 2
    (do
      (reset! cart-2 (set-pricing @cart-2 test-pricing))
      (reset! cart-2 (scan @cart-2 test-items-2))
      (my-test  (calculate-total @cart-2) 7.25))

    ;; test 3
    (do
      (reset! cart-3 (set-pricing @cart-3 test-pricing))
      (reset! cart-3 (scan @cart-3 test-items-3))
      (my-test (calculate-total @cart-3) 15.40))
    ))

;; To run in the REPL uncomment below and C-c C-k:
;; (-main)
