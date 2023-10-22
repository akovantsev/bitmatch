(ns com.akovantsev.bitmatch.impl
  (:require
   ;[#?(:cljs cljs.pprint :clj clojure.pprint) :as pp]
   [clojure.walk :as walk]
   ;[com.akovantsev.pp :as pipi]
   [clojure.string :as str]
   [clojure.walk :as walk]))

(set! *print-namespace-maps* false)
;(defn spy [x] (pp/pprint x) x)
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
       (binding [pipi/*limit-seq-elements* nil]
         (println (str ~pref# (pipi/string ~x#) "\n")))
       ~x#)))


(defmacro locals [& banned]
  ;; https://gist.github.com/noisesmith/3490f2d3ed98e294e033b002bc2de178
  (let [ks (->> &env keys
             (remove #{'_})
             (remove #(re-matches #".+__\d+" (name %)))
             (remove (set banned)))]
    (zipmap (map #(list 'quote %) ks) ks)))


(def ^:dynamic *truthy* '#{1 T +})
(def ^:dynamic *falsy*  '#{0 F -})
(def ^:dynamic *any*    '#{_ .})



(defn -contains? [coll x]
  (or (contains? coll x)
      (contains? coll (-> x str first str symbol))))

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
  (let [maxlen  (fn [syms] (->> syms (map pr-str) (map count) (reduce max 0)))
        lens    (->> pairs (map first) (apply map vector) (map maxlen))
        pp-pair (fn [[pred return]]
                  (str "  [" (str/join " " (map padright lens pred)) "]  " return))]
    (->> pairs (map pp-pair) (str/join "\n"))))




(defn bitmatch [predicates pairs precise? debug?]
  (assert (-> predicates vector?))
  (assert (-> pairs count even?))
  (let [len        (count predicates)
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
        !implicit  (atom [])

        +-         (fn [x] [(boolean x) (not x)])
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
                             (and -then +else) (swap! !unhandled conj path+)
                             (and +then -else) (swap! !unhandled conj path-)
                             (and +then +else +same -done) (make path_ then)
                             (and +then +else +same +done) (get nums then)
                             (and +then +else -same -done) (list 'if pred (make path+ then) (make path- else))
                             (and +then +else -same +done) (list 'if pred (get nums then) (get nums else))))

                         ENUM
                         (let [enum   (not-empty (dissoc m 'DEFAULT 'TRUTHY 'FALSY))
                               [+enum -enum]  (+- enum)
                               domain (get domains idx)
                               immpli (->> domain
                                        (remove (-> enum keys set))
                                        (map #(conj path %)))
                               kvdone (fn f6 [[k v]] [k (get nums v)])
                               kvmake (fn f7 [[k v]] [k (make (conj path k) v)])]
                           ;(spy (locals))
                           ;(spy [t +enum +defa -defa +done defa path path_])
                           (when defa
                             (swap! !implicit concat immpli))
                           (cond
                             (and -enum -defa) (throw (ex-info (str "oops: (and -done -enum -def) " path " " m) {}))
                             (and +enum -defa) (swap! !unhandled conj path_)
                             (and +enum +defa -done) (doall (concat ['case pred] (mapcat kvmake enum) [(make path_ defa)]))
                             (and +enum +defa +done) (doall (concat ['case pred] (mapcat kvdone enum) [(get nums defa)]))
                             (and -enum +defa -done) (make path_ defa)
                             (and -enum +defa +done) (get nums defa))))))

        code       (make [] m)
        tree       (walk/postwalk-replace nums m)
        padrdot    (fn [vek] (subvec (into vek (repeat len '.)) 0 len))
        unhandled  (map padrdot @!unhandled)
        implicit   (map padrdot @!implicit)
        deduped    (->> pairs (map (fn [[pred return]] [pred (nums return)])))
        unhstr     (pretty-branches-str (map vector unhandled (repeat "unhandled")))
        implstr    (pretty-branches-str (map vector implicit (repeat "handled implicitly")))
        dedupedstr (pretty-branches-str deduped)
        msg        (str "(bitmatch " predicates
                     (when (seq unhandled)
                       (str "\n error:\n" unhstr "\n"))
                     (when (seq implicit)
                       (str "\n warning:\n" implstr "\n"))
                     "\n declared:\n" dedupedstr
                     ")")]
    ;(spy (locals m))
    (when debug?
      (println msg))
    (if (empty? unhandled)
      (with-meta code {::tree  (list 'quote tree)})
      (throw (ex-info msg {'unhandled unhandled
                           'implicit  implicit
                           'tree      tree})))))


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

