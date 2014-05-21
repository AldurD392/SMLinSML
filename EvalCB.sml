use "Datatype.sml";

(* Valutatore di booleani come interi. *)
fun EvalBool b = if b then 1 else 0;

(* Valutatore per le costanti. *)
fun EvalConst (KInt(i)) = i
	| EvalConst (KBool(b)) = EvalBool(b)
;

(* Valutatore per costanti che ritorna booleani. *)
fun EvalIntAsBool (i) = if i > 0 then true else false;

(*
fun ValuesToTuple(EVClosure(x, y, z)) =
	(x, y, z);

*)

(* TODO: Non dovremmo mai cercare il valore di una chiusura, no? *)
fun EvalValue (EVIArray(i)) =
		i
(*	| EvalValue (EVClosure(s, p, a)) =
		a*)
;
