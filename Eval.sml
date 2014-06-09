(* Importiamo. *)
use "Tools.sml";

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

	| EvalAll (Array(v, mArray, p), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		let
			val valuesList =
				map
				(fn var => EvalAllValue(EvalM(var, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
				(arrayToList mArray)
			val locationsStoreTuple = NewArrayLocation(valuesList, StoreList (s, (sl, value)))
		in
			EvalAll(
			    	p,
			    	EnvList(EnvList (e, (x, el)), (v, EVIArray(Array.fromList(#1 (locationsStoreTuple))))),
			    	#2 (locationsStoreTuple)
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

	| EvalAll (Call(y, arg), EnvList (e, (x, el)), StoreList (s, (sl, value))) =
		let
			val location = NewLocation(StoreList (s, (sl, value)))
		in
			let
			 	val tuple = ValuesToTuple(EvalEnv(y, EnvList (e, (x, el))))
			in
			 	EvalAll(
			 	    #2 tuple,
			 	    EnvList (
			 	        #3 tuple,
			 	        (
			 	        	#1 tuple,
			 	        	EVIArray(Array.fromList([location]))
			 	        )
			 	    ),
			 	    StoreList(
						StoreList (s, (sl, value)),
						(location, EvalAllValue(EvalM(arg, EnvList (e, (x, el)), StoreList (s, (sl, value)))))
					)
			 	)
			end
		end
;
