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

(*
    CALL BY VALUE,
    let x = 0 in array y = [4, 5, 6] in proc z(w) is x := 1; y[w] := 42 in z(x)
*)
(*let
    val program =
        Variable("x", Const(KInt(0)),
            Array("y", Array.fromList([Const(KInt(4)), Const(KInt(5)), Const(KInt(6))]),
                    Proc(
                        "z",
                        "w",
                        Concat(
                            Assign(Var("x"), Const(KInt(1))),
                            Assign(Arr("y", LeftV(Var("w"))), Const(KInt(42)))
                        ),
                        Call("z", LeftV(Var("x")))
                    )
                )
            )
in
    PrintStore(EvalAll(program, e, s))
end;
*)
(*
    CALL BY REFERENCE,
    let x = 0 in array y = [4, 5, 6] in proc z(w) is x := 1; y[w] := 42 in z(x)
*)
let
    val program =
        Variable("x", Const(KInt(0)),
            Array("y", Array.fromList([Const(KInt(4)), Const(KInt(5)), Const(KInt(6))]),
                    Proc(
                        "z",
                        "w",
                        Concat(
                            Assign(Var("x"), Const(KInt(1))),
                            Assign(Arr("y", LeftV(Var("w"))), Const(KInt(42)))
                        ),
                        Call("z", Var("x"))
                    )
                )
            )
in
    PrintStore(EvalAll(program, e, s))
end;
