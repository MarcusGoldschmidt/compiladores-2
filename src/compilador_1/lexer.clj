(ns compilador-1.lexer)

(def keys ["program"
           "ident"
           "begin"
           "real"
           "if"
           "then"
           "else"
           "integer"])

(def stop-words [" "
                 "\t"
                 "\n"])

(def symbols ["*"
              ";"
              "*"
              "+"
              "-"
              "/"
              "<"
              ">"
              ":="
              "="
              "<>"
              ">="
              ","
              "."
              "("
              ")"
              "$"])

(def token-list [[#"^\d*(\.\d{1,}){0,1}" :number]
                 [#"^[a-zA-Z0-9]+" :idem]])

(defn- create-token-update-text [match type text]
  {:match match
   :type  type
   :text (subs text (count match))})

(defn- create-error [text]
   {:match ""
   :text  text
   :type  :error
   :error "Syntax error"})

(defn create-eof []
  {:match ""
   :text  ""
   :type  :eof})

(defn re-find-first
  [regex text]
  (let [match (re-find regex text)]
    (if (instance? java.lang.String match)
      match
      (first match))))

(defn exact-match-symbols [text symbols]
  (->> symbols
       (filter #(clojure.string/starts-with? text %))
       (vec)
       (reduce #(if (> (count %1) (count %2)) %1 %2))))

(defn some-starts-with? [text array-list]
  (true? (some #(clojure.string/starts-with? text %) array-list)))

(defn remove-first-word [text]
  (let [stop-word (re-find-first #"^( |\n|\t)*" text)
        new-text  (subs text (count stop-word))]
    new-text))

(defn map-token-and-peek-text
  "Peek the first value of regex and returm a set of
  value of regex, the type and the text"
  [regexs text]
  (cond
    (clojure.string/blank? text) (create-eof)
    (some-starts-with? text symbols) (create-token-update-text (exact-match-symbols text symbols) :symbol text)
    (some-starts-with? text keys) (create-token-update-text (exact-match-symbols text keys) :key text)
    :else (loop [r regexs]
            (if-let [[x & xs] r]
              (let [token   (create-token-update-text (re-find-first (first x) text) (second x) text)
                    match (:match token)]
                (if (not (clojure.string/blank? match))
                  token
                  (recur xs)))
              (create-error text)))))

(defn map-tokens
  [text]
  (lazy-seq
    (let [new-text (remove-first-word text)
          result   (map-token-and-peek-text token-list new-text)
          text     (:text result)
          token    (dissoc result :text)]
      (if (and (nil? (:error result)) (not= :eof (:type result)))
        (cons token (map-tokens text))
        (cons token (lazy-seq))))))


(take 3
 (map-tokens "program teste
	real a,b;
	integer c, d;
begin
	read(a);
	read(c);
  b := a * a + a;
  d := 2*c + c
  if a > b then
  	write(a)
  else
  	write(b)
  $;
  if d < c then
  	write(d)
  else
  	write(c)
  $
end.
"))