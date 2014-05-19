(* Il tipo per il linguaggio Imp. *)

(* Le costanti (aka i valori). *)
datatype K =
	KInt of int
	| KBool of bool

(* Le epsressioni *)
datatype Exp =
	Const of K
	| Var of string
	| Plus of Exp * Exp
	| Less of Exp * Exp
	| Greater of Exp * Exp
	| Equal of Exp * Exp
;

(* Il linguaggio *)
datatype Imp =
	Skip
	| Concat of Imp * Imp
	| If of Exp * Imp * Imp
	| While of Exp * Imp
	| Variable of string * Exp * Imp
	| Assign of string * Exp
;

(* L'ambiente, vuoto o una locazione. *)
datatype Env =
	EnvEmpty
	| EnvList of Env * (string * int)
;

(* Lo store, vuoto oppure un valore. *)
datatype Store =
	StoreEmpty
	| StoreList of Store * (int * K)
;
