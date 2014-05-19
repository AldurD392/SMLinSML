(* Importiamo. *)
use "FunDatatype.sml";

fun Max (a, b) =
	if a > b then a else b;

(* Funzione per le nuove locazioni dello store. *)
fun NewLocation (StoreEmpty) =
		1
	| NewLocation (StoreList (s, (sl, value))) =
		Max(NewLocation(s), sl) + 1
;

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
fun EvalStore (inputLocation, StoreEmpty) =
		raise StoreException
	| EvalStore (inputLocation, StoreList(s, (l, value))) =
		if l = inputLocation then
			value
		else
			EvalStore(inputLocation, s)
;

(* Valutatore per i booleani. *)
fun EvalBool b =
	if b then 1 else 0
;

(* Valutatore per le costanti. *)
fun EvalConst (KInt(i)) = i
	| EvalConst (KBool(b)) = EvalBool(b)
;

(* Valutatore per costanti che ritorna booleani. *)
fun EvalConstAsBool (KInt(i)) = if i > 0 then true else false
	| EvalConstAsBool (KBool(b)) = b
;

(*
fun ValuesToTuple(VClosure(x, y, z)) =
	(x, y, z);

*)

(* Valutatore per le espressioni (EXP). *)

(*
	ENV = EnvList (e, (x, el))
	STORE = StoreList (s, (sl, value))
*)
fun EvalExp (Const(k), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		k
	| EvalExp (Var(v), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		(* TODO: handle exception? *)
		(* TODO: handle EnvEmpty? *)
		EvalStore(
			(EvalEnv(
			    v, EnvList (e, (x, el))
			    )),
			StoreList (s, (sl, value))
		)
	| EvalExp (Plus(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		KInt(
		    EvalConst(EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))) +
		    EvalConst(EvalExp(n, EnvList (e, (x, el)), StoreList (s, (sl, value))))
		)
	| EvalExp (Less(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		KBool(
		    EvalConst(EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))) <
		    EvalConst(EvalExp(n, EnvList (e, (x, el)), StoreList (s, (sl, value))))
		)
	| EvalExp (Greater(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		KBool(
		    EvalConst(EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))) >
		    EvalConst(EvalExp(n, EnvList (e, (x, el)), StoreList (s, (sl, value))))
		)
	| EvalExp (Equal(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		KBool(
		    EvalConst(EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))) =
		    EvalConst(EvalExp(n, EnvList (e, (x, el)), StoreList (s, (sl, value))))
		)
;

fun EvalImp	(Skip, EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		StoreList (s, (sl, value))

	| EvalImp (Concat(p, q), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		EvalImp	(q, EnvList (e, (x, el)), EvalImp (p, EnvList (e, (x, el)), StoreList (s, (sl, value))))

	| EvalImp (If(m, p, q), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		if EvalConstAsBool(EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))) then
			EvalImp (p, EnvList (e, (x, el)), StoreList (s, (sl, value)))
		else
			EvalImp (q, EnvList (e, (x, el)), StoreList (s, (sl, value)))

	| EvalImp (While(m, p), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		if EvalConstAsBool(EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))) then
			EvalImp(While(m, p), EnvList (e, (x, el)), EvalImp (p, EnvList (e, (x, el)), StoreList (s, (sl, value))))
		else
			StoreList (s, (sl, value))

	| EvalImp (Variable(v, m, p), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		let
			val l = NewLocation(StoreList (s, (sl, value)))
		in
			EvalImp(
		        p,
		        EnvList(
		            EnvList (e, (x, el)),
		            (v, l)
		        ),
				StoreList(
				    StoreList (s, (sl, value)),
					(
						l,
						EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))
					)
				)
			)
		end

	| EvalImp (Assign(v, m), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		StoreList(
		    StoreList(s, (sl, value)),
		    (
		    	EvalEnv(v, EnvList (e, (x, el))),
		    	EvalExp(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))
		   	)
		)
;
