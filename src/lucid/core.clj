(ns lucid.core
  (:require [clojure.string :as str])
  (:gen-class))

;; ----------------------------
;; Lexer and Parser
;; ----------------------------

(defn tokenize [code]
  (->> (str/split-lines code)
       (map #(str/trim %))
       (remove empty?)
       (map #(str/split % #"\s+"))))

(defn parse-expr [expr tokens]
  (cond
    (re-matches #"^\d+$" expr) {:type :number :value (Integer/parseInt expr)}
    (re-matches #"^\w+$" expr) {:type :var :name expr}
    :else
    ;; Handle simple binary operations
    (let [[a op b] tokens]
      {:type :bin-op
       :op op
       :left (parse-expr a [])
       :right (parse-expr b [])})))

(defn parse [tokens]
  (let [parse-line
        (fn [tokens]
          (cond
            (= (first tokens) "let")
            ;; Variable declaration: let x = expr
            {:type :var-decl
             :name (second tokens)
             :value (parse-expr (nth tokens 3) (drop 3 tokens))}

            (= (first tokens) "func")
            ;; Function declaration: func name(params) { ... }
            (let [[_ name params & body] tokens
                  params-clean (-> params
                                   (str/replace #"[(),]" "")
                                   (str/split #","))]
              {:type :func-decl
               :name name
               :params (map str/trim params-clean)
               :body (vec (map parse-line body))})

            (= (first tokens) "return")
            ;; Return statement: return expr
            {:type :return
             :value (parse-expr (second tokens) (rest tokens))}

            ;; Expression (e.g., function call)
            {:type :expr
             :expr (parse-expr (first tokens) tokens)}))]
    (map parse-line tokens)))

;; ----------------------------
;; Evaluator
;; ----------------------------

(defn eval-expr [expr env]
  (cond
    (= (:type expr) :number) (:value expr)
    (= (:type expr) :var) (get env (:name expr))
    (= (:type expr) :bin-op)
    (let [a (eval-expr (:left expr) env)
          b (eval-expr (:right expr) env)]
      (case (:op expr)
        "+" (+ a b)
        "-" (- a b)
        "*" (* a b)
        "/" (/ a b)
        (throw (Exception. (str "Unknown operator: " (:op expr))))))
    :else (throw (Exception. (str "Unknown expression type: " expr)))))

(defn eval-node [node env]
  (cond
    (= (:type node) :var-decl)
    (assoc env (:name node) (eval-expr (:value node) env))

    (= (:type node) :func-decl)
    (assoc env (:name node) node)

    (= (:type node) :expr)
    ;; Handle function call
    (let [expr (:expr node)]
      (if (= (:type expr) :bin-op)
        (eval-expr expr env)
        (let [func (get env (:name expr))]
          (if (nil? func)
            (throw (Exception. (str "Undefined function: " (:name expr))))
            ;; Assume it's a function call
            (let [args (rest (first (:expr node)))]
              ;; Minimal handling: not implemented
              ;; For simplicity, assume functions have single return statement
              (throw (Exception. "Function calls not implemented in this minimal interpreter")))))))

    (= (:type node) :return)
    ;; Handle return statement
    (throw (ex-info "Return" {:return (eval-expr (:value node) env)}))

    :else
    (throw (Exception. (str "Unknown node type: " node)))))

(defn eval-block [nodes env]
  (try
    (reduce (fn [current-env node]
              (eval-node node current-env))
            env
            nodes)
    (catch clojure.lang.ExceptionInfo e
      (if (= (:message e) "Return")
        (:return (ex-data e))
        (throw e)))))

(defn eval-expr-full [expr env]
  (eval-expr expr env))

(defn eval-node-full [node env]
  (cond
    (= (:type node) :var-decl)
    (assoc env (:name node) (eval-expr (:value node) env))

    (= (:type node) :func-decl)
    (assoc env (:name node) node)

    (= (:type node) :expr)
    (let [expr (:expr node)]
      (if (= (:type expr) :bin-op)
        (eval-expr expr env)
        (let [func (get env (:name expr))]
          (if (nil? func)
            (throw (Exception. (str "Undefined function: " (:name expr))))
            ;; Function call
            (let [args [] ;; Minimal: No arguments handling
                  ;; Not implemented
                  0))))

    :else
    env))

;; ----------------------------
;; Interpreter
;; ----------------------------

(defn interpret [code]
  (let [tokens (tokenize code)
        ast (parse tokens)]
    (eval-block ast {})))

;; ----------------------------
;; Sample Usage
;; ----------------------------

(def sample-code
  "
  let x = 10
  let y = x + 5
  func add(a, b) {
      return a + b
  }
  let result = add(x, y)
  ")

(defn -main []
  (try
    (let [final-env (interpret sample-code)]
      (println "Final Environment:")
      (doseq [[k v] final-env]
        (if (= (:type v) :func-decl)
          (println k ": <function>")
          (println k ":" v))))
    (catch Exception e
      (println "Error during interpretation:" (.getMessage e)))))
