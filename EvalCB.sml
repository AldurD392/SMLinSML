use "Datatype.sml";

(* Valutatore di booleani come interi. *)
exception InvalidCondition of K
fun EvalBool (KBool(b)) = b
	| EvalBool (KInt(i)) = raise InvalidCondition(KInt(i))
;

(* Valutatore per le costanti. *)
exception KIntegerException of K
fun EvalConst (KInt(i)) = i
	| EvalConst (KBool(b)) = raise KIntegerException(KBool(b))
;

(* Print delle costanti. *)
fun PrintK (KInt(i)) = Int.toString(i)
	| PrintK (KBool(b)) = Bool.toString(b)
;

(* Valutatore per locazioni di ALL: *)
exception AllValueNotLocation of AllValue
fun EvalAllLocation (Location(i)) = i
	| EvalAllLocation (Constant(k)) = raise AllValueNotLocation(Constant(k))
;

(* Valutatore per valori di ALL: *)
exception AllValueNotValue of AllValue
fun EvalAllValue (Constant(c)) = c
	| EvalAllValue (Location(i)) = raise AllValueNotValue(Location(i))
;

(* Estrai i valori da una EVClousure. *)
exception EnvValueNotClosure of EnvValue
fun ClosureToTuple(EVClosure(x, y, z)) = (x, y, z)
	| ClosureToTuple(EVIArray(a)) = raise EnvValueNotClosure(EVIArray(a))
;

(* Estrai l'arrai da un EVIArray. *)
exception EnvValueNotArray of EnvValue
fun EvalValue (EVIArray(i)) = i
	| EvalValue (EVClosure(s, p, a)) = raise EnvValueNotArray(EVClosure(s, p, a))
;
