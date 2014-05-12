(* Importiamo il tipo. *)
use "FunDatatype.sml";

(* Valutatore per ambienti. *)
fun EvalEnv (inputVariable, EnvEmpty) =
		VNone
	| EvalEnv (inputVariable, EnvList(e, (x, value))) =
		if x = inputVariable then
			value
		else
			EvalEnv(inputVariable, e)
;

(* Valutatore per i valori.
Questa funzione andrà usata alla fine per valutare il valore prodotto da Eval.
Si devono considerare tutti e tre i casi, ma si dovrebbe arrivare a valutare soltanto costanti.
*)

fun EvalValues VNone =	0
	| EvalValues (VConst(c)) = c
	| EvalValues (VClosure(x, f, e)) = 0
;

fun ValuesToTuple(VClosure(x, y, z)) =
	(x, y, z);

(* Valutatore per il linguaggio Fun. *)
fun Eval (K(k), EnvList (e, (x, value))) =
		VConst (k)
	| Eval (K(k), EnvEmpty) =
			VConst (k)

	| Eval (Var(v), EnvList (e, (x, value))) =
		EvalEnv (v, EnvList(e, (x, value)))
	| Eval (Var(v), EnvEmpty) =
		EvalEnv (v, EnvEmpty)


	| Eval (Plus(p, q), EnvList (e, (x, value))) =
		VConst(EvalValues(Eval (p, EnvList(e, (x, value)))) + EvalValues(Eval(q, EnvList(e, (x, value)))))
	| Eval (Plus(p, q), EnvEmpty) =
		VConst(EvalValues(Eval (p, EnvEmpty)) + EvalValues(Eval(q, EnvEmpty)))

	| Eval (Let(var, m, n), EnvList(e, (x, value))) =
		Eval(n, EnvList(EnvList(e, (x, value)), (var, Eval(m, EnvList(e, (x, value))))))
	| Eval (Let(var, m, n), EnvEmpty) =
		Eval(n, EnvList(EnvEmpty, (var, Eval(m, EnvEmpty))))

	| Eval (Fun(var, m), EnvList(e, (x, value))) =
		VClosure (var, m, EnvList(e, (x, value)))
	| Eval (Fun(var, m), EnvEmpty) =
		VClosure (var, m, EnvEmpty)

	| Eval (App (f, var), EnvList(e, (x, value))) =
		Eval(
		    #2 (ValuesToTuple(Eval(f, EnvList(e, (x, value))))),
		    EnvList(
		    	#3 (ValuesToTuple(Eval(f, EnvList(e, (x, value))))),
		    	(#1 (ValuesToTuple(Eval(f, EnvList(e, (x, value))))),
		    		Eval(var, EnvList(e, (x, value)))
		    	)
		    )
		)
	| Eval (App (f, var), EnvEmpty) =
		Eval(
		    #2 (ValuesToTuple(Eval(f, EnvEmpty))),
		    EnvList(
		    	#3 (ValuesToTuple(Eval(f, EnvEmpty))),
		    	(#1 (ValuesToTuple(Eval(f, EnvEmpty))),
		    		Eval(var, EnvEmpty)
		    	)
		    )
		)
;

