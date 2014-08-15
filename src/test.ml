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

let sep_by sep pr =
  let rec go out ls =
    match ls with
      [] -> ()
    | l :: ls ->
      Printf.fprintf out "%a%s%a" pr l sep go ls
  in
  go

let rec run_commands cmds cp =
  match cmds with
    [] -> (true, cp)
  | (n,c) :: cmds ->
    begin
      match Coq_interact.interp n c cp with
	None -> (false, cp)
      | Some cp ->
	begin
	  let goals = Coq_interact.all_goals cp in
	  Printf.printf "Goals:\n%a" (sep_by "\n" print_goal) goals ;
	  run_commands cmds cp
	end
    end

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
