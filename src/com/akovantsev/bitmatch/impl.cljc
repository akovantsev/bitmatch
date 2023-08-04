(ns com.akovantsev.bitmatch.impl
  (:require
   #_[com.akovantsev.pp :as pipi]
   [clojure.string :as str]
   [clojure.walk :as walk]))

;(set! *print-namespace-maps* false)
#_
(defmacro spy [x]
  (let [x#    (gensym)
        path# (or (-> x meta :file)  ;;for cljs
                (-> *file* (str/split #"/") (->> (drop-while (complement #{"src"})) rest (str/join "/"))))
        ;//# sourceURL=links.lights.cljs
        pref# (format ";; %s %s:%s\n;; %s\n"
                path#
                (-> x meta :line)
                (-> x meta :column)
                (pr-str x))
        cljs?#  (:ns &env)
        srcUrl# (when cljs?# (list 'js* (str "//# sourceURL=" path#)))]
    `(let [~x# ~x]
       ~srcUrl#
       (println (str ~pref# (pipi/string ~x#) "\n"))
       ~x#)))


(def ^:dynamic *truthy* '#{1 T +})
(def ^:dynamic *falsy*  '#{0 F -})
(def ^:dynamic *any*    '#{_ .})


(defn vagueness [xs] (->> xs (filter *any*) count))

(defn by-specificity [x y]
  (compare
    [(vagueness x) (pr-str x)]
    [(vagueness y) (pr-str y)]))

(sort [1 0 nil])

(group-by vagueness '[[0 _] [0 1] [_ _] [1 1] [_ 1]])

(sort by-specificity '[[0 _] [0 1] [_ _] [1 1] [_ 1]])


(defn TRUTHY? [x] (contains? *truthy* x))
(defn FALSY? [x] (contains? *falsy* x))
(defn BOOL? [x] (or (TRUTHY? x) (FALSY? x)))
(defn ANY? [x] (contains? *any* x))


(defn get-type [pred]
  (cond
    (->> pred (remove ANY?) empty?)                'UNUSED
    (->> pred (remove ANY?) (remove BOOL?) empty?) 'BOOL
    :else                                          'ENUM))


(defn get-types [preds]
  (->> preds (apply map vector) (map set) (mapv get-type)))


(assert
  (= '[ENUM BOOL BOOL UNUSED BOOL ENUM]
    (get-types
      '[[. . . . . .]
        [. + . . + b]
        [a . - . - .]
        [c . - . + +]])))


(defn expand [types xs]
  (loop [typs types
         todo xs
         done []]
    (let [[x & todo-] todo
          [t & typs-] typs]
      (if (empty? todo)
        [done]
        (case t
          UNUSED (recur typs- todo- (conj done 'DEFAULT))
          ENUM   (recur typs- todo- (conj done (if (ANY? x) 'DEFAULT x)))
          BOOL   (cond
                   (TRUTHY? x)    (recur typs- todo- (conj done 'TRUTHY))
                   (FALSY? x)     (recur typs- todo- (conj done 'FALSY))
                   (not (ANY? x)) (recur typs- todo- (conj done x))
                   :else          (mapcat (partial expand types)
                                    [(-> done (conj 'TRUTHY) (into todo-))
                                     (-> done (conj 'FALSY) (into todo-))])))))))



(let [types ['ENUM 'BOOL 'UNUSED 'BOOL 'ENUM 'ENUM 'ENUM]]
  (apply map vector types (expand types '[:yo 1 _ 0 _ :sup 1])))

(assert (= (expand ['ENUM] [:yo]) [[:yo]]))
(assert (= (expand ['ENUM] [1])   [[1]]))
(assert (= (expand ['ENUM] '[_])  [['DEFAULT]]))

(binding [*any* '#{*}]
  (assert (= (expand ['ENUM] '[*]) [['DEFAULT]])))


(defn bitmatch [predicates pairs precise?]
  (assert (-> predicates vector?))
  (assert (-> pairs count even?))
  (let [len        (count predicates)
        pairs      (partition 2 pairs)
        triples    (map-indexed vector pairs)

        split      (fn split [[pred idx]]
                     (let [preds (->> pred (partition-all len) (remove empty?) (map vec))]
                       (assert (->> preds (map count) (every? #{len})) {'len len 'preds preds})
                       (map vector preds (repeat idx))))

        nums       (->> triples (map (fn f3 [[idx [pred then]]] [idx then])) (into {}))
        pairs      (->> triples (map (fn f4 [[idx [pred then]]] [pred idx])) (mapcat split) (sort-by first by-specificity))

        types      (->> pairs (map first) get-types)
        duplicates (->> pairs (map first) frequencies (remove #(-> % val (= 1))) (map key))

        _          (when-not (empty? duplicates)
                     (let [f    #(-> % vec print with-out-str (str " duplicate"))
                           rows (str/join "\n  " (map f duplicates))
                           msg  (str "(bitmatch " predicates "\n  " rows ")")]
                       (throw (ex-info msg {}))))

        reg        (fn reg [m [pred then]]
                     (let [preds     (expand types pred)
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

        m          (reduce reg {} pairs)

        !unhandled (atom [])

        +-         (fn [x] [(boolean x) (not x)])
        make       (fn make [path m]
                     (let [defa  (get m 'DEFAULT)
                           idx   (count path)
                           t     (get types idx)
                           pred  (get predicates idx)
                           idx+  (inc idx)
                           [+defa -defa]  (+- defa)
                           [+done -done] (+- (= idx+ len))
                           path_ (conj path '.)
                           path1 (conj path '+)
                           path0 (conj path '-)]
                       (case t
                         UNUSED
                         (cond
                           -done (make path_ defa)
                           +done (get nums defa))

                         BOOL
                         (let [then  (get m 'TRUTHY)
                               else  (get m 'FALSY)
                               [+then -then] (+- then)
                               [+else -else] (+- else)
                               [+same -same] (+- (= then else))]
                           ;(spy [t +then +else +same +done])
                           (cond
                             (and -then -else) (throw (ex-info (str "oops: (and -then -else) " path) {}))
                             (and -then +else) (swap! !unhandled conj path1)
                             (and +then -else) (swap! !unhandled conj path0)
                             (and +then +else +same -done) (make path_ then)
                             (and +then +else +same +done) (get nums then)
                             (and +then +else -same -done) (list 'if pred (make path1 then) (make path0 else))
                             (and +then +else -same +done) (list 'if pred (get nums then) (get nums else))))

                         ENUM
                         (let [enum   (not-empty (dissoc m 'DEFAULT 'TRUTHY 'FALSY))
                               [+enum -enum]  (+- enum)
                               kvdone (fn f6 [[k v]] [k (get nums v)])
                               kvmake (fn f7 [[k v]] [k (make (conj path k) v)])]
                           ;(spy [t +enum +defa +done])
                           (cond
                             (and -enum -defa) (throw (ex-info (str "oops: (and -done -enum -def) " path " " m) {}))
                             (and +enum -defa) (swap! !unhandled conj path_)
                             (and +enum +defa -done) (concat ['case pred] (mapcat kvmake enum) [(make path_ defa)])
                             (and +enum +defa +done) (concat ['case pred] (mapcat kvdone enum) [(get nums defa)])
                             (and -enum +defa -done) (make path_ defa)
                             (and -enum +defa +done) (get nums defa))))))

        code       (make [] m)
        unhandled  @!unhandled]

    (if (empty? unhandled)
      (with-meta code {::tree  (list 'quote (walk/postwalk-replace nums m))})
      (let [ext  identity ;#(vec (take len (concat % (repeat '.))))
            rows (str/join "\n  " (map str (map ext unhandled) (repeat " unhandled")))
            msg  (str "(bitmatch " predicates "\n  " rows ")")]
        (throw (ex-info msg {'unhandled unhandled}))))))
