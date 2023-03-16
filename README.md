## What

Yet another cond-like macro. 

```clojure
(macroexpand-1
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
``` 

## Why

Prunes branches: above, notice just 1 `(baz? x)` test instead of 4.

Throws at compile time:
- if there are unhandled cases (`case` throws at runtime, `cond` returns nil)
  ```clojure
   (bitmatch [foo bar baz]
     [1 0 _] a)
  
   Syntax error macroexpanding bitmatch at...
   (bitmatch [foo bar baz]
     [1 1 _] unhandled
     [0 _ _] unhandled)
   ```
- if there are exact literal pattern duplicates (written by hand), e.g:
  ```clojure
  (bitmatch [foo bar baz]
    [1 0 _] a
    [1 0 _] b)
  
  Syntax error macroexpanding bitmatch at...
    (bitmatch [foo bar baz]
      [1 0 _] duplicate)
  ```

Does not throw or warn
- if there are 2 different but matching patterns, e.g:
  ```clojure
  (bitmatch [foo bar baz]
    [1 0 _] a
    [1 0 1] b
    ...)
  ```
  but uses more specific one: `b`.

Pattern is more specific if (in comparator significance order):
1) it has fewer `_` (wildcards),
2) they occur further to the right in the pattern:
   ```clojure
   (sort by-specificity [[0 _] [0 1] [_ _] [1 1] [_ 1]])
   ;=> ([0 1] [1 1] [0 _] [_ 1] [_ _])
    ```

This means:
- clauses' order does not matter – group and reorder for readability
- predicates' order matters:
    - for execution order: which pred is tested first
    - for pattern specificity: may affect wich out of several matching
      patterns will actually be used, e.g:
      ```clojure
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
      ```
    Sometimes this makes things hard to reason about, but so does `cond` clauses' order ¯\_(ツ)_/¯.


## Install 

```clojure
;; in deps.edn
{:deps {github-akovantsev/bitmatch
        {:git/url "https://github.com/akovantsev/bitmatch"
         :sha     ""}}} ;; actual sha
```