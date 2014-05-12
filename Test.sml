(* Importiamo il valutatore. *)
use "FunEval.sml";

(* Creiamo un ambiente di prova. *)
val e = EnvList(EnvValue("x", Const(3)), EnvValue("y", Const(5)));

(* Proviamo la somma. *)
Eval(
    Plus(Var("x"), Var("y")), e
);

Eval(
    Let("x", K(Const(3)),
        Let("y", K(Const(5)), Plus(Var("x"), Var("y")))
    ), EnvList(EnvEmpty, EnvEmpty)
);
