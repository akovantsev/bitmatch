(ns com.akovantsev.bitmatch.impl
  (:require
   ;[#?(:cljs cljs.pprint :clj clojure.pprint) :as pp]
   [clojure.walk :as walk]
   [akovantsev.pp :as pp]
   [clojure.string :as str]
   [clojure.walk :as walk]))

(set! *print-namespace-maps* false)


(def ^:dynamic *truthy* '#{1 T +})
(def ^:dynamic *falsy*  '#{0 F -})
(def ^:dynamic *any*    '#{_ .})



(defn -contains? [coll x]
  (or (contains? coll x)
      (and (symbol? x)
           (contains? coll (-> x str first str symbol)))))

(defn TRUTHY? [x] (-contains? *truthy* x))
(defn FALSY? [x] (-contains? *falsy* x))
(defn BOOL? [x] (or (TRUTHY? x) (FALSY? x)))
(defn ANY? [x] (-contains? *any* x))


(defn by-specificity [aa bb]
  (cond
    (empty? aa) 0
    (empty? bb) 0
    :else
    (let [a (first aa)
          b (first bb)]
      (cond
        ;; when a=. b=....
        (and (ANY? a) (ANY? b))  (recur (rest aa) (rest bb))
        (ANY? a) +1
        (ANY? b) -1
        (= a b)  (recur (rest aa) (rest bb))
        :else    (compare (str a) (str b))))))



(sort-by identity by-specificity
;(map specificity
 '[[:word \} .] [. . nil] [. \# .]
   [. \( ..] [. \) .] [. \; .]
   [. \[ ..] [. \] .] [. \{ ..]
   [. \} .] [..... \' .]
   [..... \newline .] [..... \space +] [..... \space .]
   [:comm . .] [:err . .] [:str . .]
   [:vec . .] [. . .]])


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

(assert
  (= '[ENUM BOOL BOOL UNUSED BOOL ENUM]
    (get-types
      '[[. .... .... . . .]
        [. +foo .... . + b]
        [a .... -bar . - .]
        [c .... -bar . + +]])))


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


(defn cartesian [& colls]
  (loop [todo (->> colls rest)
         done (->> colls first (map vector))]
    (if (empty? todo)
      done
      (let [[xs & todo-] todo]
        (recur todo- (mapcat
                       (fn [ds] (map #(conj ds %) xs))
                       done))))))


(cartesian [1 2 3] [:a :b] [\c \d])


(defn ensure-list [x] (if (or (list? x) (seq? x)) x (list x)))

(defn unroll-cartesian [pred]
  (->> pred (map ensure-list) (apply cartesian) (reduce into [])))


(assert
  (= '(1 . 4, 1 . 5, 2 . 4, 2 . 5, 3 . 4, 3 . 5)
     (unroll-cartesian
       '[(1 2 3) . (4 5)])))



(defn padright [n x] (subs (apply str (pr-str x) (repeat n " ")) 0 n))
(padright 10 ::foo)
(padright 10 :foo)


(defn pretty-branches-str [pairs]
  (let [maxlen   (fn [syms] (->> syms (map pr-str) (map count) (reduce max 0)))
        lens     (->> pairs (map first) (apply map vector) (map maxlen))
        str-pred (fn [pred] (str "[" (str/join " " (map padright lens pred)) "]"))
        pp-pair  (fn [[pred return]]
                   (str "  " (str-pred pred) "  "
                     (if (-> return meta ::handler)
                       (str-pred return)
                       return)))]
    (->> pairs (map pp-pair) (str/join "\n"))))




(defn bitmatch [predicates pairs precise? mmeta]
 (assert (-> predicates vector?))
 (assert (-> pairs count even?))
 (binding [*any*    (-> mmeta :*any* (or *any*))
           *falsy*  (-> mmeta :*falsy* (or *falsy*))
           *truthy* (-> mmeta :*truthy* (or *truthy*))]
  (let [debug?     (-> mmeta :tag boolean)
        len        (count predicates)
        pairs      (partition 2 pairs)
        triples    (map-indexed vector pairs)

        split      (fn split [[pred idx]]
                     (assert (-> pred count (mod len) zero?) {'len len 'pred pred})
                     (let [preds (->> pred
                                   (partition-all len)
                                   (mapcat unroll-cartesian)
                                   (partition-all len)
                                   (remove empty?)
                                   (map vec))]
                       (map vector preds (repeat idx))))

        nums       (->> triples (map (fn f3 [[idx [pred then]]] [idx then])) (into {}))
        pairs      (->> triples (map (fn f4 [[idx [pred then]]] [pred idx])) (mapcat split) (sort-by first by-specificity))

        preds      (->> pairs (map first))
        types      (->> preds get-types)
        duplicates (->> preds frequencies (remove #(-> % val (= 1))) (map key))
        domains    (->> preds (apply map vector)
                     (mapv (fn [t d]
                             (if (= t 'BOOL) '#{+ -} (remove ANY? (distinct d))))
                       types))

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
        !implicit  (atom {})

        +-         (fn [x] [(boolean x) (not x)])
        done       (if debug?
                     (fn done [p v] (list 'do (list 'println (pr-str p)) (get nums v)))
                     (fn done [p v] (get nums v)))
        make       (fn make [path m]
                     (let [defa  (get m 'DEFAULT)
                           idx   (count path)
                           t     (get types idx)
                           pred  (get predicates idx)
                           idx+  (inc idx)
                           [+defa -defa] (+- defa)
                           [+done -done] (+- (= idx+ len))
                           path_ (conj path '.)
                           path+ (conj path '+)
                           path- (conj path '-)]

                       (case t
                         UNUSED
                         (cond
                           -done (make path_ defa)
                           +done (done path_ defa))

                         BOOL
                         (let [then  (get m 'TRUTHY)
                               else  (get m 'FALSY)
                               [+then -then] (+- then)
                               [+else -else] (+- else)
                               [+same -same] (+- (= then else))]
                           ;(spy [t +then +else +same +done])
                           (cond
                             (and -then -else) (throw (ex-info (str "oops: (and -then -else) " path) {}))
                             (and -then +else) (swap! !unhandled conj path+)
                             (and +then -else) (swap! !unhandled conj path-)
                             (and +then +else +same -done) (make path_ then)
                             (and +then +else +same +done) (done path_ then)
                             (and +then +else -same -done) (list 'if pred (make path+ then) (make path- else))
                             (and +then +else -same +done) (list 'if pred (done path+ then) (done path- else))))

                         ENUM
                         (let [enum   (not-empty (dissoc m 'DEFAULT 'TRUTHY 'FALSY))
                               [+enum -enum]  (+- enum)
                               domain (get domains idx)
                               immpli (->> domain
                                        (remove (-> enum keys set))
                                        (map #(conj path %)))
                               kvdone (fn f6 [[k v]] [k (done (conj path k) v)])
                               kvmake (fn f7 [[k v]] [k (make (conj path k) v)])]
                           ;(spy (locals))
                           ;(spy [t +enum +defa -defa +done defa path path_])
                           (if defa
                             (swap! !implicit merge (zipmap immpli (repeat path_)))
                             (swap! !unhandled concat immpli))
                           (cond
                             (and -enum -defa) (throw (ex-info (str "oops: (and -done -enum -def) " path " " m) {}))
                             (and +enum -defa) (swap! !unhandled conj path_)
                             (and +enum +defa -done) (doall (concat ['case pred] (mapcat kvmake enum) [(make path_ defa)]))
                             (and +enum +defa +done) (doall (concat ['case pred] (mapcat kvdone enum) [(done path_ defa)]))
                             (and -enum +defa -done) (make path_ defa)
                             (and -enum +defa +done) (done path_ defa))))))

        code       (make [] m)
        tree       (walk/postwalk-replace nums m)
        padrdot    (fn [vek] (subvec (into vek (repeat len '.)) 0 len))
        unhandled  (->> @!unhandled (map padrdot) (sort by-specificity))
        unhandled2 (map vector unhandled (repeat "UNHANDLED"))
        deduped    (->> pairs (map (fn [[pred return]] [pred (nums return)])))
        implicit   (->> @!implicit (map (fn [[pred handler]] [(padrdot pred) (with-meta (padrdot handler) {::handler true})])))
        all        (sort-by first by-specificity (concat deduped implicit unhandled2))
        unhstr     (pretty-branches-str unhandled2)
        msg        (str "(bitmatch " predicates
                     (when (seq unhandled)
                       (str "\n error:\n" unhstr "\n"))
                     "\n all:\n" (pretty-branches-str all)
                     ")")]
    (when debug?
      (println msg)
      (println)
      (pp/$ (pp/locals +- reg padrdot make done split !unhandled !implicit)))
    (if (empty? unhandled)
      (with-meta code {::tree  (list 'quote tree)})
      (throw (ex-info msg {'unhandled unhandled
                           'all       all
                           'tree      tree}))))))


(defn substitute [groups lst]
  (loop [todo lst
         del? false
         kip  []
         del  #{}]
    (if (empty? todo)
      (distinct (remove del kip))
      (let [x  (first todo)
            vv (get groups x [x])]
        (if (= x '-)
          (recur (rest todo) true kip del)
          (if del?
            (recur (rest todo) del? kip (into del vv))
            (recur (rest todo) del? (into kip vv) del)))))))


(defn with-subs [pairs body]
  (assert (->> pairs count even?) pairs)
  (assert (->> pairs (partition 2) (map second) (every? coll?)))
  (let [groups (->> pairs
                 (partition 2)
                 (reduce
                   (fn [pairs [sym expr]]
                     (conj pairs sym (with-subs pairs expr)))
                   [])
                 (apply hash-map))

        names  (-> groups keys set)

        wf     (fn wf [form]
                 (if (and (list? form) (some names form))
                   (substitute groups form)
                   form))]

    (with-meta
      (walk/postwalk wf body)
      (meta body))))


#_
(with-subs '[a [1 2 3]
             b [4 5 6]
             c (3 6 a - b)
             d (a b c 7 - 5)]
  '(bitmatch [foo bar]
     [(0 a b - 3 4) (a d - c)] pew))

