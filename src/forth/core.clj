(ns forth.core
  (use forth.forth))

(defn reload
  []
  (use 'forth.core :reload-all))