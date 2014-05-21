use "EvalCB.sml";

(* Valutatore per ambienti. *)
exception EnvException
fun EvalEnv (inputVariable, EnvEmpty) =
		raise EnvException
	| EvalEnv (inputVariable, EnvList(e, (x, l))) =
		if x = inputVariable then
			l
		else
			EvalEnv(inputVariable, e)
;

exception StoreException
(* Valutatore per Store. *)
fun EvalStore (inputLocation, StoreEmpty) =
		raise StoreException
	| EvalStore (inputLocation, StoreList(s, (l, value))) =
		if l = inputLocation then
			value
		else
			EvalStore(inputLocation, s)
;
