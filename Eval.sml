(* Importiamo. *)
use "EvalVM.sml";

(* Funzione per il massimo. *)
fun Max (a, b) = if a > b then a else b;

(* Funzione per le nuove locazioni dello store. *)
fun NewLocation (StoreEmpty) =
		1
	| NewLocation (StoreList (s, (sl, value))) =
		Max(NewLocation(s), sl) + 1
;

(* Valutatore per il linguaggio All. *)
fun EvalAll	(Skip, EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		StoreList (s, (sl, value))

	| EvalAll (Concat(p, q), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		EvalAll	(q, EnvList (e, (x, el)), EvalAll (p, EnvList (e, (x, el)), StoreList (s, (sl, value))))

	| EvalAll (If(m, p, q), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		if EvalBool(EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value))))) then
			EvalAll (p, EnvList (e, (x, el)), StoreList (s, (sl, value)))
		else
			EvalAll (q, EnvList (e, (x, el)), StoreList (s, (sl, value)))

	| EvalAll (While(m, p), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		if EvalBool(EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value))))) then
			EvalAll(While(m, p), EnvList (e, (x, el)), EvalAll (p, EnvList (e, (x, el)), StoreList (s, (sl, value))))
		else
			StoreList (s, (sl, value))

	| EvalAll (Variable(v, m, p), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		let
			val l = NewLocation(StoreList (s, (sl, value)))
		in
			EvalAll(
		        p,
		        EnvList(
		            EnvList (e, (x, el)),
		            (v, EVIArray(Array.fromList([l])))
		        ),
				StoreList(
				    StoreList (s, (sl, value)),
					(
						l,
						EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value))))
					)
				)
			)
		end

	| EvalAll (Assign(v, m), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		StoreList(
		    StoreList(s, (sl, value)),
		    (
		    	EvalAllLocation(EvalV(v, EnvList (e, (x, el)), StoreList (s, (sl, value)))),
		    	EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value))))
		   	)
		)

	| EvalAll (Proc(y, arg, p, q), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		EvalAll(
			q,
			EnvList(
			    EnvList(e, (x, el)),
			    (y, EVClosure(arg, p, EnvList(e, (x, el))))
			),
			StoreList (s, (sl, value))
		)
;
