(ns com.akovantsev.bitmatch
  #?(:cljs (:require-macros [com.akovantsev.bitmatch :refer [bitmatch]]))
  (:require [com.akovantsev.bitmatch.impl :as impl]))


(defmacro bitmatch
  "(macroexpand-1
     '(bitmatch [(foo? x) (bar? y) (baz? x)]
        [0 _ 1
         1 _ _] a
        [0 1 _] c
        [1 1 _] b
        [_ _ _] :default))
   ;=>
   (if (foo? x)
     (if (bar? y) b a)
     (if (bar? y) c (if (baz? x) a :default)))

   Prunes branches: above, notice just 1 (baz? x) test instead of 4.

   Throws at compile time:
   - if there are unhandled cases (case throws at runtime, cond returns nil)
   - if there are exact literal (written by hand) pattern duplicates, e.g:
     (bitmatch [foo bar baz]
       [1 0 _] a
       [1 0 _] b)

   Does not throw or warn
   - if there are 2 different but matching patterns, e.g:
     (bitmatch [foo bar baz]
       [1 0 1] a
       [1 0 _] b)
     but uses more specific one.

   Pattern is more specific if (in comparator significance order):
   1) it has fewer _ (wildcards),
   2) they occur further to the right in the pattern:
     (sort by-specificity [[0 _] [0 1] [_ _] [1 1] [_ 1]])
     ;=> ([0 1] [1 1] [0 _] [_ 1] [_ _])

   This means:
   - clauses' order does not matter – group and reorder for readability
   - predicates' order matters:
     - for execution order: which pred is tested first
     - for pattern specificity: may affect wich out of several matching
       patterns will actually be used, e.g:
        (bitmatch [foo bar]      vs.  (bitmatch [foo bar]
          [1 _] a                       [_ 1] a
          [_ 1] b                       [1 _] b
          [0 0] d)                      [0 0] d)

        (if foo a (if bar b d))  vs.  (if bar b (if foo a d))

        which is equivalent to:
        (if foo (if bar a a) (if bar b d))
        (if bar (if foo b b) (if foo a d))

        which is equivalent to:
        (if foo (if bar a a) (if bar b d))
                        ^----------------- [1 1] a
        (if foo (if bar b a) (if bar b d))
                        ^----------------- [1 1] b
        "
  [predicates & pairs]
  (impl/bitmatch predicates pairs false (meta &form)))


(defmacro with-subs [pairs body]
  `~(impl/with-subs pairs body))



#_
(let [x 1]
  ^?
  ^{:*truthy* #{t}}
  (bitmatch [x]
    [t] 1
    [-] 2))
#_
(macroexpand-1 '
  (with-subs [a [1 2 3]
              b [4 5 6]
              c (3 6 a - b)
              d (a b c 7 - 5)]
    (bitmatch [foo bar]
      [(0 a b - 3 4) (a d - c)] pew)))

#_
(with-subs [two [\a \b]]
  (macroexpand-1 '
     ^?
     (bitmatch [\a \b \c]
       [(:Aff  .) (two .) -
        :C          .       .]
       true
       ;[:B \a +]  'kek
       [:B \b +]  'sup
       ;[.  . +]  :yo

       [:B . .
        . . .]
       :wow

       ;[:Aff (two .) +]
       ;false

       [. (two) +]
       (some default))))

#_
(macroexpand-1 '
        ^?
  (bitmatch [a b c]
    [. . .] :1
    [9 9 9] :2
    [9 9 .] :4
    [9 . .] :3))

#_
(let [f #(let [a % b 2 c 3]
           ^?
           (bitmatch [a b c true]
             [. . . -] :11
             [. . . +] :1
             [9 9 9 .] :2
             [9 9 . .] :4
             [9 . . +] :3
             [9 . . -] :33))]
  (f 9)
  (f 1))
