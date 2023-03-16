(ns com.akovantsev.bitmatch.impl
  (:require 
   [clojure.string :as str]
   [clojure.walk :as walk]))


(assoc-in {} "abs" 1)

(defn vagueness [x] (->> x (filter #{\_ '_ }) count))

(defn by-specificity [x y]
  (compare
    [(vagueness x) x]
    [(vagueness y) y]))

(sort [1 0 nil])

(group-by vagueness ["0_" "01" "11" "_1" "__"])

(sort by-specificity ["0_" "01" "__" "11" "_1"])

(defn expand [s]
  (let [t (str/replace-first s "_" "1")
        f (str/replace-first s "_" "0")]
    (if (= t f)
      [t]
      (mapcat expand [t f]))))

(expand "1_1_1")



(defn bitmatch [predicates pairs precise?]
  (assert (-> predicates vector?))
  (assert (-> pairs count even?))
  (let [len        (count predicates)
        pairs      (partition 2 pairs)
        triples    (map-indexed vector pairs)

        split      (fn split [[pred idx]]
                     (if-not (vector? pred)
                       [[pred idx]]
                       (if (-> pred first string?)
                         (mapv vector pred (repeat idx))
                         (let [preds (->> pred (split-at len) (remove empty?) (map str/join))]
                           (map vector preds (repeat idx))))))

        nums       (->> triples (map (fn f3 [[idx [pred then]]] [idx then])) (into {}))
        pairs      (->> triples (map (fn f4 [[idx [pred then]]] [pred idx])) (mapcat split) (sort-by first by-specificity))

        duplicates (->> pairs (map first) frequencies (remove #(-> % val (= 1))) (map key))

        _          (when-not (empty? duplicates)
                     (let [f #(-> % vec print with-out-str (str " duplicate"))
                           rows (str/join "\n  " (map f duplicates))
                           msg  (str "(bitmatch " predicates "\n  " rows ")" predicates rows)]
                       (throw (ex-info msg {}))))

        reg        (fn reg [m [pred then]]
                     (assert (re-matches #"[10_]+" pred) pred)
                     (let [preds     (expand pred)
                           maybe-set (fn maybe-set [m pred]
                                       (let [regged (get-in m pred)
                                             debug  {'predicates predicates 'pred pred 'then then 'regged (get nums regged)}]
                                         (assert (-> pred count (= len)) debug)
                                         (if-not regged
                                           (assoc-in m pred then)
                                           (if precise?
                                             (throw (ex-info (str "duplicate predicate: " pred) debug))
                                             m))))]
                       (reduce maybe-set m preds)))

        wf         (fn wf [x]
                     (if-not (map? x)
                       x
                       (let [t (get x \1)
                             f (get x \0)]
                         (if (= t f)
                           (when t {\= t})
                           x))))

        m          (->> pairs (reduce reg {}) (walk/postwalk wf))
        !unhandled (atom [])

        make       (fn make [path m]
                     (let [same  (get m \=)
                           then  (get m \1)
                           else  (get m \0)
                           idx   (count path)
                           pred  (get predicates idx)
                           idx+  (inc idx)
                           done  (= idx+ len)
                           path_ (conj path '_)
                           path1 (conj path 1)
                           path0 (conj path 0)]
                       (cond
                         (every? nil? [same then else])
                         (swap! !unhandled conj path)

                         same
                         (if done
                           (get nums same)
                           (make path_ same))

                         :else
                         (if done
                           (list 'if pred
                             (if then (get nums then) (swap! !unhandled conj path1))
                             (if else (get nums else) (swap! !unhandled conj path0)))
                           (list 'if pred
                             (make path1 then)
                             (make path0 else))))))
        code       (make [] m)
        unhandled  @!unhandled]

    (if (empty? unhandled)
      code
      (let [rows (str/join "\n  " (map str unhandled (repeat " unhandled")))
            msg  (str "(bitmatch " predicates "\n  " rows ")")]
        (throw (ex-info msg {'unhandled unhandled}))))))
