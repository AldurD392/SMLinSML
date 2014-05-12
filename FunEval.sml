(* Importiamo il tipo. *)
use "FunDatatype.sml";

(* Valutatore per ambienti. *)
fun EvalEnv (inputVariable, EnvEmpty) =
		VNone
	| EvalEnv (inputVariable, EnvValue(variable, value)) =
		if inputVariable = variable then
			VConst (value)
		else
			VNone
	| EvalEnv (inputVariable, EnvClosure(variable, expression, enviroment)) =
		if inputVariable = variable then
			VClosure (variable, expression, enviroment)
		else
			VNone
	| EvalEnv (inputVariable, EnvList(eone, etwo)) =
		if not (EvalEnv(inputVariable, etwo) = VNone) then
			EvalEnv(inputVariable, etwo)
		else
			EvalEnv(inputVariable, eone)
;

(* Valutatore per costanti. *)
fun EvalConst (Const(c)) = c;

(* Valutatore per i valori degli ambienti. *)
fun EvalValues VNone =	0
	| EvalValues (VConst(c)) = EvalConst(c)
	| EvalValues (VClosure(x, f, e)) = 0
;

(* Valutatore per variabili. *)
fun EvalVar (Var(v), EnvList(e1, e2)) =
	EvalValues (EvalEnv (v, EnvList(e1, e2)))
;

(* Valutatore per funzioni. *)
fun EvalFun (Fun(x, m), EnvList(e1, e2)) =
	EnvClosure(x, m, EnvList(e1, e2))
;

(* Valutatore per il linguaggio Fun. *)
fun Eval (K(k), EnvList (e1, e2)) =
		EvalConst (k)
	| Eval (Var(v), EnvList (e1, e2)) =
		EvalVar (Var(v), EnvList (e1, e2))
	| Eval (Plus(p, q), EnvList (e1, e2)) =
		Eval (p, EnvList(e1, e2)) + Eval(q, EnvList(e1, e2))
	| Eval (Let(x, m, n), EnvList(e1, e2)) =
		Eval(n, EnvList(EnvList(e1, e2), EnvValue(x, Const(Eval(m, EnvList(e1, e2))))))
	| Eval (Fun(x, m), EnvList(e1, e2)) =
		EvalValues (VClosure (x, m, EnvList(e1, e2)))
	(*| Eval (App (f, x), EnvList(e1, e2)) =*)
;
