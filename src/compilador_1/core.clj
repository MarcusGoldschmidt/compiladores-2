(ns compilador-1.core
  (:require [compilador-1.syntactic :as syntactic]
            [compilador-1.lexer :as lex]))

(defn run [text]
  (let [tokens    (lex/map-tokens text)
        syntactic (syntactic/run-syntactic tokens)]
    syntactic))

(->
 (run "program teste
	real: a,b;
	integer: c, d;
begin
    read(a);
    read(c);
  b := a * a + a;
  d := 2*c + c;
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
")
 (dissoc :tokens))

(->
 (run "program teste
	real: a,b;
begin
	read(a)
end.")
 (dissoc :tokens))

(doall
  (lex/map-tokens "program teste
	real: a,b;
	integer: c, d;
begin
	read(a)
end."))