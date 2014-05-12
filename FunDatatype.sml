(* Il tipo per il linguaggio Fun. *)
datatype Fun =
	K of int
	| Var of string
	| Plus of Fun * Fun
	| Let of string * Fun * Fun
	| Fun of string * Fun
	| App of Fun * Fun
;

(* L'ambiente, può essere un valore (cioè una costante o una chiusura). *)
datatype 'a Env =
	EnvEmpty
	| EnvList of 'a Env * (string * 'a)
;

datatype Values =
	VNone
	| VConst of int
	| VClosure of (string * Fun * Values Env)
;
