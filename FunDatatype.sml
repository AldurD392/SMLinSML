(* Le costanti, sono semplicemente gli interi (e inseriamo l'elemento nullo per comodità). *)
datatype Constant =
	Const of int
;

(* Il tipo per il linguaggio Fun. *)
datatype Fun =
	K of Constant
	| Var of string
	| Plus of Fun * Fun
	| Let of string * Fun * Fun
	| Fun of string * Fun
	| App of Fun * Fun
;

(* L'ambiente, può essere un valore (cioè una costante o una chiusura). *)
datatype Env =
	EnvEmpty
	| EnvValue of string * Constant
	| EnvClosure of string * Fun * Env
	| EnvList of Env * Env
;

datatype Values =
	VNone
	| VConst of Constant
	| VClosure of string * Fun * Env
;
