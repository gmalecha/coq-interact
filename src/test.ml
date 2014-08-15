open Coq_interact

let commands_True =
  [(None,"Goal True")
  ;(None,"exact I")
  ;(None,"Qed")]

let commands_plus_0 =
  [(None,"Goal forall n : nat, n + 0 = n")
  ;(None,"induction n")
  ;(None,"reflexivity")
  ;(None,"simpl")
  ;(None,"rewrite IHn")
  ;(None,"reflexivity")
  ;(None,"Qed")
  ]

let rec run_commands cmds cp =
  match cmds with
    [] -> (true, cp)
  | (n,c) :: cmds ->
    match Coq_interact.interp n c cp with
      None -> (false, cp)
    | Some cp -> run_commands cmds cp

let main () =
  match Coq_interact.init () with
  | Some cp ->
    let (res,cp) = run_commands commands_True cp in
    assert res ;
    let (res,cp) = run_commands commands_plus_0 cp in
    assert res ;
    print_string "finished! (no errors)\n"
  | None ->
    print_string "failed to start coqtop\n"
;;
main ()
