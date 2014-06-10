use "EvalES.sml";

(* Valutatore per le espressioni sinistre. *)
fun EvalV (Var(v), env, store) =
		Location(
		    Array.sub(EvalValue(EvalEnv(v, env)), 0)
		)
	| EvalV (Arr(a, m), env, store) =
		Location(
		    Array.sub(
		        EvalValue(EvalEnv(a, env)),
		        EvalConst(EvalAllValue(EvalM(m, env, store)))
		    )
		)
and

(* Valutatore per le espressioni destre. *)
	EvalM (Const(k), env, store) =
		Constant(k)

	| EvalM (LeftV(v), env, store) =
		Constant(
			EvalStore(
				EvalAllLocation(EvalV (v, env, store)),
				store
			)
		)

	| EvalM (Plus(m, n), env, store) =
	    Constant(
	        KAdd(
			    EvalAllValue(EvalM(m, env, store)),
			    EvalAllValue(EvalM(n, env, store))
			)
	    )

	| EvalM (Less(m, n), env, store) =
		Constant(
			KBool(
			    EvalConst(EvalAllValue(EvalM(m, env, store)))
			    <
			    EvalConst(EvalAllValue(EvalM(n, env, store)))
			)
		)

	| EvalM (Greater(m, n), env, store) =
		Constant(
			KBool(
			    EvalConst(EvalAllValue(EvalM(m, env, store)))
			    >
			    EvalConst(EvalAllValue(EvalM(n, env, store)))
			)
		)

	| EvalM (Equal(m, n), env, store) =
		Constant(
			KBool(
			    EvalConst(EvalAllValue(EvalM(m, env, store)))
			    =
			    EvalConst(EvalAllValue(EvalM(n, env, store)))
			)
		)
;
