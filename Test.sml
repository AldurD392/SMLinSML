(* Importiamo il valutatore. *)
use "Eval.sml";

(* Creiamo un ambiente di prova. *)
val e = EnvList(
            EnvList(
            	EnvEmpty, ("x", EVIArray(Array.fromList([1])))
        	), ("y", EVIArray(Array.fromList([2, 4])))
);

val s = StoreList(
            StoreList(
                StoreEmpty, (1, KInt(10))
            ), (2, KInt(20))
);

(* Proviamo a valutare una espressione sinistra: *)
EvalV (
        Arr("y", Const(KInt(1))),
        e,
        s
    );

(* Proviamo la somma. *)
(*EvalConst(
    EvalExp(
        Greater(Var("x"), Var("y")), e, s
    )
);
*)
(*EvalImp(
    Variable(
        "x",
        Const(KInt(7)),
        Variable(
            "y",
            Const(KInt(9)),
            If(
               Greater(
                    Var("x"),
                    Var("y")
                ),
               Concat(
                   Assign(
                        "y",
                        Plus(
                            Var("y"),
                            Const(KInt(1))
                        )
                    ),
                   Assign(
                        "x",
                        Plus(
                            Var("x"),
                            Const(KInt(1))
                        )
                    )
               ),
               Assign(
                    "x",
                    Plus(
                        Var("x"),
                        Var("y")
                    )
                )
            )
        )
    ),
    e,
    s
);*)
