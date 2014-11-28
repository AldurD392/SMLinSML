(* Il tipo per il linguaggio Imp. *)

(* Le costanti (aka i valori). *)
datatype K =
	KInt of int
	| KBool of bool
;

exception AdditionException
fun KAdd(KInt x, KInt y) = KInt(x + y)
	| KAdd(x, y) = raise AdditionException
;

(* I left value. *)
datatype V =
	Var of string
	| Arr of string * M

and

(* I right value. *)
M =
	Const of K
	| LeftV of V
	| Plus of M * M
	| Less of M * M
	| Greater of M * M
	| Equal of M * M
;

(* Il linguaggio ALL *)
datatype All =
	Skip
	| Concat of All * All
	| If of M * All * All
	| While of M * All
	| Variable of string * M * All
	| Array of string * M array * All
	| Assign of V * M
	| Proc of string * string * All * All
	(*| Call of string * M*)
	| Call of string * V
;

(* Possibili valori dell'ambiente: array di intero o chiusura. *)
datatype EnvValue =
	EVIArray of int array
	| EVClosure of string * All * Env
and

(* L'ambiente, vuoto o un valore. *)
Env =
	EnvEmpty
	| EnvList of Env * (string * EnvValue)
;

(* Lo store, vuoto oppure un valore. *)
datatype Store =
	StoreEmpty
	| StoreList of Store * (int * K)
;

(* Wrapper dei valori di riorno delle funzioni EvalVM *)
datatype AllValue =
	Location of int
	| Constant of K
;
