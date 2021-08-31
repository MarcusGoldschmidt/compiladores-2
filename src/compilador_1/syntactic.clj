(ns compilador-1.syntactic
  (:require [compilador-1.lexer :as lex]))

(declare dc variaveis comandos condicao expressao fator outros_termos espressao pfalse)

(defmacro def-syntax-pipeline [name & pipes]
  `(defn ~name []
    (fn [x#]
      (validate-syntax x# ~@pipes))))

(macroexpand
 '(def-syntax-pipeline programa
   (-check-value "program")
   (-check-is-identifier)
   (corpo)
   (-check-value ".")))

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
         (if (= (key-to-compare token) value)
           (assoc map :tokens (drop 1 tokens))
           (-create-erro (drop 1 tokens) token value))
         (-create-erro (drop 1 (:tokens map)) (lex/create-eof) value)))))
  ([value] (-check-value value :match)))

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
      map
      (-create-erro (drop 1 (:tokens map)) (lex/create-eof) :empty))))

(defn- -or-check-values [& args]
  (fn [map]
    (loop [syntax args
           errors []]
      (if-let [[x & xs] syntax]
        (let [result (apply validate-syntax (into [map] (vec x)))]
          (if (nil? (:error result))
            result
            (recur xs (conj errors result))))
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
   ; TODO(Marcus) Validar tipo numero
   [(-check-is-number)]
   [(-check-value "(")
    (espressao)
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
  (outros_termos))


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
  (-check-value ";")
  (dc))

(def-syntax-pipeline dc
  (-or-check-values
   [(dc-v) (mais-dc)]
   [(-check-empty-chain)]))

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