use "EvalES.sml";

(* Valutatore per le espressioni sinistre. *)
fun EvalV (Var(v), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		Location(
		    Array.sub(EvalValue(EvalEnv(v, EnvList (e, (x, el)))), 0)
		)
	| EvalV (Arr(a, m), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		Location(
		    Array.sub(
		        EvalValue(EvalEnv(a, EnvList (e, (x, el)))),
		        EvalConst(EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
		    )
		)
and

(* Valutatore per le espressioni destre. *)
	EvalM (Const(k), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		Constant(k)

	| EvalM (LeftV(v), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		Constant(
			EvalStore(
				EvalAllLocation(EvalV (v, EnvList (e, (x, el)), StoreList (s, (sl, value)))),
				StoreList (s, (sl, value))
			)
		)

	| EvalM (Plus(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
	    Constant(
	        KInt(
			    EvalConst(EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
			    +
			    EvalConst(EvalAllValue(EvalM(n, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
		    )
	    )

	| EvalM (Less(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		Constant(
			KBool(
			    EvalConst(EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
			    <
			    EvalConst(EvalAllValue(EvalM(n, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
			)
		)

	| EvalM (Greater(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		Constant(
			KBool(
			    EvalConst(EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
			    >
			    EvalConst(EvalAllValue(EvalM(n, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
			)
		)

	| EvalM (Equal(m, n), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		Constant(
			KBool(
			    EvalConst(EvalAllValue(EvalM(m, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
			    =
			    EvalConst(EvalAllValue(EvalM(n, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
			)
		)
;
