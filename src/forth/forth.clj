(ns forth.forth
  (use clojure.repl))

(def stack (atom ()))
(def ret-stack (atom ()))
(def forth-mode (atom :interp))
(def forth-input (atom ""))
;;Dictonary entries have the following keys:
;;  :fn or :subwords if primitive or not
;;  :primitive?
;;  :immediate?
;;  :name
(def dict (atom ()))

(defn do-pop
  ([stack]
     (do-pop stack true))
  ([stack ignore-empty?]
     (let [item (first @stack)]
       (if ignore-empty?
         (swap! stack pop)
         (if (not (empty? @stack))
           (swap! stack pop)))
       item)))

(defn do-push
  [stack item]
  (swap! stack conj item))

(def push-stack (partial do-push stack))
(def pop-stack (partial do-pop stack))
(def push-ret-stack (partial do-push ret-stack))
(def pop-ret-stack (partial do-pop ret-stack false))
(def push-dict (partial do-push dict))
(def pop-dict (partial do-pop dict))

(defn dict-find
  [word]
  (loop [cur @(first @dict)
         dict (rest @dict)
         from-bottom (count dict)]
    (if (= (:name cur) word)
      {:word cur :xt (bit-shift-left from-bottom 16)}
      (if (empty? dict)
        nil
        (recur
         @(first dict)
         (rest dict)
         (dec from-bottom))))))

(defn create
  ([name]
   (create name nil))
  ([name {:keys [primitive? immediate? fn]}]
     (let [subwords (if primitive? nil {:subwords []})]
       (push-dict (atom
                   (merge
                    {:name name
                     :primitive? primitive?
                     :immediate? immediate?}
                    (if primitive?
                      {:fn fn}
                      {:subwords []})))))))

(defn create-prim
  ([name f]
     (create-prim name f false))
  ([name f immediate?]
     (create name {:primitive? true
                   :immediate? immediate?
                   :fn f})))

;;xts are as follows:
;;  lower 16 bits -> subword
;;  upper 47/48 bits -> index from bottom of dictionary stack
(defn xt->dict-idx
  [xt]
  (let [dict-size (count @dict)
        from-bottom (bit-shift-right xt 16)]
    (if (< from-bottom dict-size)
      (- (dec dict-size) from-bottom)
      nil)))
(defn xt->subword
  [xt]
  (bit-and xt 0xFFFF))

(defn xt->info
  [xt]
  (let [dict-idx (xt->dict-idx xt)
        subword (xt->subword xt)]
    (if dict-idx
      {:word @(nth @dict dict-idx)
       :subword subword}
      nil)))

(defn xt-info->subword-xt
  [info]
  (nth (:subwords (:word info))
       (:subword info)))

(defn next-xt
  "Returns the next xt, or nil if nothing left"
  ([xt num]
     (let [new-xt (+ xt num)
           new-info (xt->info new-xt)]
       (println (format "next-xt new-info: %s-%s ret-stack: %s"
                        (:name (:word new-info))
                        (:subword new-info)
                        @ret-stack))
       (if (<= (count (:subwords (:word new-info)))
               (:subword new-info))
         (pop-ret-stack)
         new-xt)))
  ([xt]
     (next-xt xt 1)))

(defmacro prim-fn
  [& body]
  `(fn [xt#]
     ~@body
     (next-xt xt#)))
(create-prim "dup" (prim-fn (push-stack (first @stack))))
(create-prim "+" (prim-fn
                  (let [a (pop-stack)
                        b (pop-stack)]
                    (push-stack (+ a b)))))
(create-prim "-"
             (prim-fn
              (let [a (pop-stack)
                    b (pop-stack)]
                (push-stack (- b a)))))
(create-prim "."
             (prim-fn
              (println (pop-stack))))

(declare forth-next-word)
(create-prim "create"
             (prim-fn
              (create (forth-next-word))))

(create-prim ":"
             (prim-fn
              (create (forth-next-word))
              (reset! forth-mode :compile)))
(create-prim ";"
             (prim-fn
              (reset! forth-mode :interp))
             true)
(create-prim "lit" (fn [xt]
                     (let [info (xt->info (next-xt xt))]
                       (println (format "in lit. info: %s -- num: %s" info
                                        (xt-info->subword-xt info)))
                       (push-stack (xt-info->subword-xt info))
                       (println "Next xt: " (next-xt xt 1))
                       (next-xt xt 1))))

(defn forth-read
  ([]
     (forth-read (read-line)))
  ([text]
     (reset! forth-input text)))

(defn forth-read-until
  ([re]
     (loop [input (.trim @forth-input)
            word []]
       (if (or (empty? input)
               (re-find re (apply str input)))
         (do
           (reset! forth-input (apply str input))
           (apply str word))
         (recur (rest input)
                (conj word (first input))))))
  ([]
     (forth-read-until #"^\s")))
(defn forth-next-word
  []
  (forth-read-until))

(declare do-xt)
(defn run-xt
  [xt]
  (let [info (xt->info xt)]
    (println (format "run-xt %s" (:name (:word info))))
    (if (:primitive? (:word info))
      ((:fn (:word info)) xt)
      (do
        (println (format "push-ret-stack: %s" (next-xt xt)))
        (push-ret-stack (next-xt xt))
        (xt-info->subword-xt info)))))
(defn compile-val
  [val]
  (let [top-word (first @dict)]
    (swap! top-word
           (fn [cur]
             (let [subwords (:subwords cur)
                   new-subwords (conj subwords val)]
               (assoc cur :subwords new-subwords))))))
(defn compile-xt
  [xt]
  (compile-val xt)
  (next-xt xt))

(defn- do-xt
  [xt]
  (when xt
    (let [info (xt->info xt)]
      (println (format "do-xt xt: %s name: %s subword: %s" xt (:name (:word info)) (:subword info)))
      (when info
        (case @forth-mode
          :interp
          (do 
              (do-xt (run-xt xt)))
          :compile
          (do (println "Compiling word")
              (do-xt (if (:immediate? (:word info))
                       (run-xt xt)
                       (compile-xt xt))))
          (println "Error, invalid state"))))))
(defn- handle-number
  [num]
  (let [num (try
              (Integer/valueOf num)
              (catch Exception e
                (throw (Exception. (format "Not found: %s" num)))))]
    (case @forth-mode
      :interp
      (do (push-stack num))
      :compile
      (do
        (println "Compiling number: " num)
        (compile-xt (:xt (dict-find "lit")))
        (compile-val num))
      (println "Error, invalid state"))))
(defn forth-process-word
  [word]
  (let [entry (dict-find word)]
    (println @stack)
    (println (format "Word: %s -- xt: %s" word (:xt entry)))
    (if entry
      (do-xt (:xt entry))
      (handle-number word))))
(defn forth-eval
  []
  (while (not (empty? @forth-input))
    (forth-process-word (forth-next-word))))

(defn forth-test
  ([data]
     (forth-read data)
     (forth-eval))
  ([]
     (forth-test ": foo + - ;
                  3 4 5 foo .
                  : three+ 3 + ;
                  5 three+ .")))

;;TODO -- test lit