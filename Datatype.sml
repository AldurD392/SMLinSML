(* Il tipo per il linguaggio Imp. *)

(* Le costanti (aka i valori). *)
datatype K =
	KInt of int
	| KBool of bool
;

(* I left value. *)
datatype 'a V =
	Var of string
	| Arr of string * 'a
;

(* I right value. *)
datatype M =
	Const of K
	| LeftV of M V
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
	| Assign of M V * M
	| Proc of string * string * All * All
	| Call of string * M
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
