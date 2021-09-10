(ns compilador-1.syntactic
  (:require [compilador-1.lexer :as lex]))

(declare dc variaveis comandos condicao expressao fator outros_termos expressao pfalse)

(defmacro def-syntax-pipeline [name & pipes]
  `(defn ~name []
    (fn [x#]
      (validate-syntax x# ~@pipes))))

(defn- -create-erro [tokens current-token expected]
  {:tokens        tokens
   :error         true
   :expected      expected
   :current-token current-token})

(defn- -check-value
  ([value key-to-compare]
   (fn [map]
     (let [tokens (:tokens map)]
       (if-let [token (first tokens)]
         ; lex error
         (if (= :error (:type token))
           token
           ; semantic error
           (if (= (key-to-compare token) value)
             (assoc map :tokens (drop 1 tokens))
             (if (nil? (:last-erros map))
               (-create-erro (drop 1 tokens) token value)
               (-create-erro (drop 1 tokens) token (:last-erros map)))))
         (-create-erro (drop 1 (:tokens map)) (lex/create-eof) value)))))
  ([value] (-check-value value :match)))

(nil? (:a {}))

(defn on-detect-success [x f]
  (let [result (f x)]
    (if-let [error (:error result)]
      result
      (reduced result))))

(defn on-detect-error [x f]
  (let [result (f x)]
    (if-let [error (:error result)]
      (reduced result)
      result)))

(defn validate-syntax [map & args]
  (reduce on-detect-error map args))

(defn- -check-eof []
  (-check-value :eof :eof))

(defn- -check-is-identifier []
  (-check-value :idem :type))

(defn- -check-is-number []
  (-check-value :number :type))

(defn- -check-empty-chain []
  (fn [map]
    (if-let [token (first (:tokens map))]
      (assoc map :empty-chain true)
      (-create-erro (drop 1 (:tokens map)) (lex/create-eof) :empty))))

(defn- is-valid-empty-chain-with-error [result errors xs]
  (and
   (true? (and (true? (:empty-chain result)) (> (count errors) 0) (= 0 (count xs))))))

(defn- is-valid-empty-chain-without-error [result errors xs]
  (and
   (true? (and (true? (:empty-chain result)) (= (count errors) 0) (= 0 (count xs))))))

(defn- -or-check-values [& args]
  (fn [map]
    (loop [[x & xs] args
           errors   []]
      (if (not (nil? x))
        (let [result (apply validate-syntax (into [map] (vec x)))]
          (cond
            (is-valid-empty-chain-with-error result errors xs)    (assoc result :last-erros errors)
            (nil? (:error result))                                result
            :else                                                 (recur xs (conj errors (dissoc result :tokens)))))
        (-create-erro (drop 1 (:tokens map)) (lex/create-eof) errors)))))

(defn- -fuction-statement [name]
  [(-check-value name)
   (-check-value "(")
   (-check-is-identifier)
   (-check-value ")")])

(defn- -if-statement []
  [(-check-value "if")
   (condicao)
   (-check-value "then")
   (comandos)
   (pfalse)
   (-check-value "$")])

(defn- -assignment-statement []
  [(-check-is-identifier)
   (-check-value ":=")
   (expressao)])

(def-syntax-pipeline pfalse
  (-or-check-values
   [(-check-value "else")
    (comandos)]
   [(-check-empty-chain)]))

(def-syntax-pipeline op-mul
  (-or-check-values
   [(-check-value "*")]
   [(-check-value "/")]))

(def-syntax-pipeline mais-fatores
  (-or-check-values
   [(op-mul)
    (fator)
    (mais-fatores)]
   [(-check-empty-chain)]))

(def-syntax-pipeline op-ad
  (-or-check-values
   [(-check-value "+")]
   [(-check-value "-")]))

(def-syntax-pipeline outros-termos
  (-or-check-values
   [(op-ad) (termo) (outros-termos)]
   [(-check-empty-chain)]))

(def-syntax-pipeline fator
  (-or-check-values
   [(-check-is-identifier)]
   [(-check-is-number)]
   [(-check-value "(")
    (expressao)
    (-check-value ")")]))

(def-syntax-pipeline op_un
  (-or-check-values
   [(-check-value "-")]
   [(-check-empty-chain)]))

(def-syntax-pipeline termo
  (op_un)
  (fator)
  (mais-fatores))

(def-syntax-pipeline expressao
  (termo)
  (outros-termos))


(def-syntax-pipeline relacao
  (-or-check-values
   [(-check-value "=")]
   [(-check-value "<>")]
   [(-check-value ">=")]
   [(-check-value "<=")]
   [(-check-value ">")]
   [(-check-value "<")]))

(def-syntax-pipeline condicao
  (expressao)
  (relacao)
  (expressao))

(def-syntax-pipeline comando
  (-or-check-values
   (-fuction-statement "read")
   (-fuction-statement "write")
   (-assignment-statement)
   (-if-statement)))

(def-syntax-pipeline mais-comandos
  (-or-check-values
   [(-check-value ";")
    (comandos)]
   [(-check-empty-chain)]))

(def-syntax-pipeline comandos
  (comando)
  (mais-comandos))

(def-syntax-pipeline tipo-var
  (-or-check-values
   [(-check-value "real")]
   [(-check-value "integer")]))

(def-syntax-pipeline mais-variaveis
  (-or-check-values
   [(-check-value ",")
    (variaveis)]
   [(-check-empty-chain)]))

(def-syntax-pipeline variaveis
  (-check-is-identifier)
  (mais-variaveis))

(def-syntax-pipeline dc-v
  (tipo-var)
  (-check-value ":")
  (variaveis))

(def-syntax-pipeline mais-dc
  (-or-check-values
   [(-check-value ";")
    (dc)]
   [(-check-empty-chain)]))

(def-syntax-pipeline dc
  (-or-check-values
   [(dc-v) (mais-dc)]
   [(-check-empty-chain)]))

(dissoc
 ((corpo)
   {:tokens       [{:match "real", :type :key}
                   {:match ":", :type :symbol}
                   {:match "a", :type :idem}
                   {:match ",", :type :symbol}
                   {:match "b", :type :idem}
                   {:match ";", :type :symbol}
                   {:match "begin", :type :key}
                   {:match "read", :type :idem}
                   {:match "(", :type :symbol}
                   {:match "a", :type :idem}
                   {:match ")", :type :symbol}
                   {:match "end", :type :key}
                   {:match ".", :type :key}]
    :s-tree       {}
    :symbol-table {}})
 :tokens)

(def-syntax-pipeline corpo
  (dc)
  (-check-value "begin")
  (comandos)
  (-check-value "end"))

(def-syntax-pipeline programa
  (-check-value "program")
  (-check-is-identifier)
  (corpo)
  (-check-value "."))

((programa)
  {:tokens       [{:match "program", :type :key}
                  {:match "valor", :type :idem}
                  {:match "begin", :type :key}
                  {:match "read", :type :idem}
                  {:match "(", :type :symbol}
                  {:match "a", :type :idem}
                  {:match ")", :type :symbol}
                  {:match "end", :type :key}
                  {:match ".", :type :key}]
   :s-tree       {}
   :symbol-table {}})

(defn run-syntactic [tokens]
  ((programa)
    {:tokens       tokens
     :s-tree       {}
     :symbol-table {}}))