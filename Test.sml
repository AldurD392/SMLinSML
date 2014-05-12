(* Importiamo il valutatore. *)
use "FunEval.sml";

(* Creiamo un ambiente di prova. *)
val e = EnvList(
            EnvList(
            	EnvEmpty, ("x", VConst(3))
        	), ("y", VConst(5))
);

(* Proviamo la somma. *)
Eval(
    Plus(Var("x"), Var("y")), e
);

Eval(
    Let("x", K(3),
        Let("y", K(5), Plus(Var("x"), Var("y")))
    ), EnvEmpty
);


fun ValuesToTuple(VClosure(x, y, z)) =
	(x, y, z);

VClosure("x", K(3), EnvEmpty);
#1 (ValuesToTuple(VClosure("x", K(3), EnvEmpty)));

Eval(
    App(
        Fun("x", Plus(Var("x"), K(1))),
        K(5)
    ), EnvEmpty
);





