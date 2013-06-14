(ns forth.util)

(defn update-first
  "Updates the first item in a list using the provided function and args
   first item gets passed to function at start of arguments"
  [l f & args]
  (conj (drop 1 l)
        (apply f (first l) args)))

(defn set-nth
  [l n val]
  (concat (take n l)
          [val]
          (drop (inc n) l)))
(defn update-nth
  [l n f & args]
  (set-nth l n (apply f (nth l n) args)))

