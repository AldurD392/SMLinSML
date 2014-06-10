(* Importiamo. *)
use "Tools.sml";

(* Valutatore per il linguaggio All. *)
(* Regola dello skip. *)
fun EvalAll	(Skip, env, store) =
		store

	(* Regola della concatenazione. *)
	| EvalAll (Concat(p, q), env, store) =
		EvalAll	(q, env, EvalAll (p, env, store))

	(* Regola dell'IF. *)
	| EvalAll (If(m, p, q), env, store) =
		if EvalBool(EvalAllValue(EvalM(m, env, store))) then
			EvalAll (p, env, store)
		else
			EvalAll (q, env, store)

	(* Regola del While. *)
	| EvalAll (While(m, p), env, store) =
		if EvalBool(EvalAllValue(EvalM(m, env, store))) then
			EvalAll(While(m, p), env, EvalAll (p, env, store))
		else
			store

	(* Regola della variabile. *)
	| EvalAll (Variable(v, m, p), env, store) =
		let
			val l = NewLocation(store)
		in
			EvalAll(
		        p,
		        EnvList(
		            env,
		            (v, EVIArray(Array.fromList([l])))
		        ),
				StoreList(
				    store,
					(
						l,
						EvalAllValue(EvalM(m, env, store))
					)
				)
			)
		end

	(* Regola dell'array. *)
	| EvalAll (Array(v, mArray, p), env, store) =
		let
			val valuesList =
				map
				(fn var => EvalAllValue(EvalM(var, env, store)))
				(arrayToList mArray)
			val locationsStoreTuple = NewArrayLocation(valuesList, store)
		in
			EvalAll(
			    	p,
			    	EnvList(env, (v, EVIArray(Array.fromList(#1 (locationsStoreTuple))))),
			    	#2 (locationsStoreTuple)
			    )
		end

	(* Regola dell'assegnamento. *)
	| EvalAll (Assign(v, m), env, store) =
		StoreList(
		    store,
		    (
		    	EvalAllLocation(EvalV(v, env, store)),
		    	EvalAllValue(EvalM(m, env, store))
		   	)
		)

	(* Regola della procedura. *)
	| EvalAll (Proc(y, arg, p, q), env, store) =
		EvalAll(
			q,
			EnvList(
			    env,
			    (y, EVClosure(arg, p, env))
			),
			store
		)

	(* Call by VALUE. *)
	| EvalAll (Call(y, arg), env, store) =
		let
			val location = NewLocation(store)
		in
			let
			 	val tuple = ClosureToTuple(EvalEnv(y, env))
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
						store,
						(location, EvalAllValue(EvalM(arg, env, store)))
					)
			 	)
			end
		end

	(* Call by REFERENCE. *)
(*	| EvalAll (Call(y, arg), env, store) =
		let
		 	val tuple = ClosureToTuple(EvalEnv(y, env))
		in
		 	EvalAll(
		 	    #2 tuple,
		 	    EnvList (
		 	        #3 tuple,
		 	        (
		 	        	#1 tuple,
		 	        	EVIArray(Array.fromList([EvalAllLocation(EvalV(arg, env, store))]))
		 	        )
		 	    ),
				store
		 	)
		end*)
;
