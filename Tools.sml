use "EvalVM.sml";

(* Funzione per il massimo. *)
fun Max (a, b) = if a > b then a else b;

(* Funzione per le nuove locazioni dello store. *)
fun NewLocation (StoreEmpty) =
		1
	| NewLocation (StoreList (s, (sl, value))) =
		Max(NewLocation(s), sl) + 1
;

(* Converti un array in una lista. *)
fun arrayToList s =
	map (fn i => Array.sub(s, i)) (List.tabulate(Array.length s, (fn x => x)))
;

(* Presa una lista di valori li inserisce nello store. *)
fun NewArrayLocation (nil, store) =
		(nil, store)
	| NewArrayLocation (a::lst, store) =
		let
			val newLocation = NewLocation(store)
			val newStore = StoreList(store, (newLocation, a))
			val returnStore = NewArrayLocation(lst, newStore)
		in
			(newLocation::(#1 (returnStore)), #2 (returnStore))
		end
;

fun PrintStore (StoreEmpty) =
		print "\n"
	| PrintStore (StoreList (s, (sl, value))) =
		(
			PrintStore(s);
			print (Int.toString(sl) ^ " " ^ PrintK(value) ^ "\n")
		)
;
