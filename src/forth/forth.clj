(ns forth.forth
  (use clojure.repl))

;;Dictonary entries have the following keys:
;;  :fn or :subwords if primitive or not
;;  :primitive?
;;  :immediate?
;;  :name
(defn new-vm
  []
  {:ip nil        ;;Instruction pointer
   :stack ()      ;;Data stack
   :ret-stack ()  ;;Return stack
   :mode :interp  ;;Current mode (interp or compile)
   :input ""      ;;Input buffer
   :dict ()       ;;Dictionary
   })

(defn forth-read
  ([vm]
     (forth-read (read-line)))
  ([vm text]
     (assoc vm :input text)))

(defn forth-read-until
  "Returns [<word read> <new vm state>]"
  ([vm re]
     (loop [input (.trim (:input vm))
            word []]
       (if (or (empty? input)
               (re-find re (apply str input)))
         [(apply str word)
          (assoc vm :input (apply str input))]
         (recur (rest input)
                (conj word (first input))))))
  ([vm]
     (forth-read-until vm #"^\s")))
(defn forth-next-word
  [vm]
  (forth-read-until vm))


(defn do-pop
  "Return a vector of [<popped value> <new vm state>]"
  ([stack vm]
     (do-pop stack false vm))
  ([stack check-empty? vm]
     (let [item (first (vm stack))]
       [item
        (if (not check-empty?)
          (update-in vm [stack] pop)
          (if (not (empty? (vm stack)))
            (update-in vm [stack] pop)
            vm))])))

(defn do-push
  [stack vm item]
  (update-in vm [stack] conj item))

(def push-stack (partial do-push :stack))
(def pop-stack (partial do-pop :stack))
(def push-ret-stack (partial do-push :ret-stack))
(def pop-ret-stack (partial do-pop :ret-stack true))
(def push-dict (partial do-push :dict))
(def pop-dict (partial do-pop :dict))
(defn set-forth-mode [vm mode] (assoc vm :mode mode))
(defn set-forth-ip [vm ip] (assoc vm :ip ip))

(defn dict-find
  [vm word]
  (let [dict (vm :dict)]
    (loop [cur (first dict)
           dict (rest dict)
           from-bottom (count dict)]
      (if (= (:name cur) word)
        {:word cur :xt (bit-shift-left from-bottom 16)}
        (if (empty? dict)
          nil
          (recur
           (first dict)
           (rest dict)
           (dec from-bottom)))))))

(defn create
  ([vm name]
     (create vm name nil))
  ([vm name {:keys [primitive? immediate? fn]}]
     (let [subwords (if primitive? nil {:subwords []})]
       (push-dict vm
                  (merge
                   {:name name
                    :primitive? primitive?
                    :immediate? immediate?}
                   (if primitive?
                     {:fn fn}
                     {:subwords []}))))))

(defn create-prim
  ([vm name f]
     (create-prim vm name f false))
  ([vm name f immediate?]
     (create vm name {:primitive? true
                      :immediate? immediate?
                      :fn f})))

;;xts are as follows:
;;  lower 16 bits -> subword
;;  upper 47/48 bits -> index from bottom of dictionary stack
(defn xt->dict-idx
  [vm xt]
  (let [dict-size (count (vm :dict))
        from-bottom (bit-shift-right xt 16)]
    (if (< from-bottom dict-size)
      (- (dec dict-size) from-bottom)
      nil)))
(defn xt->subword
  [xt]
  (bit-and xt 0xFFFF))

(defn xt->info
  [vm xt]
  (let [dict-idx (xt->dict-idx vm xt)
        subword (xt->subword xt)]
    (if dict-idx
      {:word (nth (vm :dict) dict-idx)
       :subword subword}
      nil)))

(defn xt-info->subword-xt
  [info]
  (when (> (count (:subwords (:word info)))
           (:subword info))
    (nth (:subwords (:word info))
         (:subword info))))

(defn forth-next
  ([vm num]
     (if (and (:ip vm)
                num)
       (update-in vm [:ip]
                  + num)
       vm))
  ([vm]
     (forth-next vm 1)))

(defn- add-subword
  [entry val]
  (update-in entry [:subwords]
             conj val))
(defn compile-val
  [vm val]
  (update-in vm [:dict]
             (fn [dict]
               (conj (drop 1 dict)
                     (add-subword (first dict) val)))))

(defn compile-word
  [vm word]
  (compile-val vm (:xt (dict-find vm word))))

(defn handle-number
  [vm num]
  (let [num (try
              (Integer/valueOf num)
              (catch Exception e
                (throw (Exception. (format "Word not found: %s" num)))))]
    (case (:mode vm)
      :interp
      (push-stack vm num)
      :compile
      (-> vm
          (compile-word "lit")
          (compile-val num)))))

(defn valid-xt?
  [vm xt]
  (let [info (xt->info vm xt)]
    (when info
      (when (or (zero? (:subword info))
                (< (:subword info)
                   (count (:subwords (:word info)))))
        true))))
(defn push-ret-if-valid
  [vm xt]
  (if (valid-xt? vm xt)
    (push-ret-stack vm xt)
    vm))
(defn run-xt
  [vm xt]
  (let [info (xt->info vm xt)]
    (println (format "run-xt %s" (:name (:word info))))
    (if (:primitive? (:word info))
      ((:fn (:word info)) vm)
      (-> vm
          (push-ret-if-valid (inc xt))
          (assoc :ip (xt-info->subword-xt info))))))

(defn forth-step
  [vm]
  (if-let [ip (:ip vm)]
    (case (:mode vm)
      :interp
      (run-xt vm ip)
      :compile
      (let [info (xt->info vm ip)]
        (if (:immediate? (:word info))
          (run-xt vm ip)
          (compile-val vm ip))))
    vm))

(defn forth-exit
  [vm]
  (let [[new-xt vm] (pop-ret-stack vm)]
    (println (format "forth-exit -- ret stack: %s" (:ret-stack vm)))
    (assoc vm :ip new-xt)))

(defn forth-process-read
  [vm]
  (let [[word vm] (forth-next-word vm)
        entry (dict-find vm word)]
    (if entry
      (case (:mode vm)
        :interp
        (assoc vm :ip (:xt entry))
        :compile
        (if (:immediate? (:word entry))
          (assoc vm :ip (:xt entry))
          (compile-val vm (:xt entry))))
      (handle-number vm word))))

(defn- has-input?
  [vm]
  (not (empty? (:input vm))))

(def ^:dynamic *delay* #(Thread/sleep 100))
(defn forth-exec-input
  ([vm]
     (forth-exec-input vm (read-line)))
  ([vm input]
     (loop [vm (forth-read vm input)]
       (when *delay*
        (*delay*))
       (println (format "ip: %s -- input: %s -- mode: %s" (:ip vm) (:input vm) (:mode vm)))
       (if (:ip vm)
         (recur (forth-step vm))
         (if (has-input? vm)
           (recur (-> vm
                      (forth-process-read)
                      (forth-step)))
           vm)))))

(defmacro prim-fn
  [& body]
  `(fn [vm#]
     (let [~'vm vm#]
       (forth-exit
        (do
          ~@body)))))
(defn add-prims
  [vm]
  (-> vm
      (create-prim "dup" (prim-fn (push-stack vm (first (vm :stack)))))
      (create-prim "over" (prim-fn (push-stack vm (second (vm :stack)))))
      (create-prim "drop" (prim-fn (let [[_ vm] (pop-stack vm)]
                                     vm)))
      (create-prim "+" (prim-fn
                        (let [[a vm] (pop-stack vm)
                              [b vm] (pop-stack vm)]
                          (push-stack vm (+ a b)))))
      (create-prim "*" (prim-fn
                        (let [[a vm] (pop-stack vm)
                              [b vm] (pop-stack vm)]
                          (push-stack vm (* a b)))))
      (create-prim "-"
                   (prim-fn
                    (let [[a vm] (pop-stack vm)
                          [b vm] (pop-stack vm)]
                      (push-stack vm (- b a)))))
      (create-prim "/"
                   (prim-fn
                    (let [[a vm] (pop-stack vm)
                          [b vm] (pop-stack vm)]
                      (push-stack vm (quot b a)))))
      (create-prim "mod"
                   (prim-fn
                    (let [[a vm] (pop-stack vm)
                          [b vm] (pop-stack vm)]
                      (push-stack vm (mod b a)))))
      (create-prim "."
                   (prim-fn
                    (let [[item vm] (pop-stack vm)]
                      (println item)
                      vm)))
      (create-prim "emit"
                   (prim-fn
                    (let [[item vm] (pop-stack vm)]
                      (print (char item))
                      vm)))
      (create-prim ".s"
                   (prim-fn
                    (println (:stack vm))
                    vm))
      (create-prim "create"
                   (prim-fn
                    (let [[cur-word vm] (forth-next-word vm)]
                      (create vm cur-word))))
      (create-prim ","
                   (prim-fn
                    (let [[item vm] (pop-stack vm)]
                      (compile-val vm item))))
      (create-prim ":"
                   (prim-fn
                    (let [[cur-word vm] (forth-next-word vm)]
                      (-> vm
                          (create cur-word)
                          (set-forth-mode :compile)))))
      (create-prim "exit" forth-exit)
      (create-prim ";"
                   (prim-fn
                    (-> vm
                        (compile-word "exit")
                        (set-forth-mode :interp)))
                   true)
      (create-prim "lit" (fn [vm]
                           (let [info (xt->info vm (inc (:ip vm)))]
                             (-> vm
                                 (push-stack (xt-info->subword-xt info))
                                 (forth-next)))))
))


(defn test-vm
  []
  (-> (new-vm)
      (add-prims)
      (push-stack 5)
      (push-stack 4)))

(defn test-prim
  [vm prim]
  ((:fn (:word (dict-find vm prim))) vm))

;;(def tmp (test-vm))
;;(test-prim (forth-read (test-vm) "foo") "create")
;;(-> (test-vm) (forth-read "foo") (test-prim "create") (test-prim "+"))

;;(forth-exec-input (-> (new-vm) (add-prims)) ": foo + - ; : bar .s foo .s ; 3 4 5 bar")
