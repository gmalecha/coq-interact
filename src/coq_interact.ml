type coq_process =
{ proc_id : int
; stdin : Unix.file_descr
; stdout : Unix.file_descr
}

type coq_string = string

type goal =
{ premises : coq_string list
; goal : coq_string
}

let print_goal out gl =
  List.iter (fun i -> Printf.fprintf out "%s\n" i) gl.premises ;
  Printf.fprintf out "======================\n" ;
  Printf.fprintf out "%s" gl.goal

type runner = int option -> string -> coq_process -> coq_process option

let coqtop = "coqtop"

let init () : coq_process option =
  let (coq_in_read, coq_in_write) = Unix.pipe () in
  let (coq_out_read, coq_out_write) = Unix.pipe () in
  let pid =
    Unix.create_process coqtop [| "coqtop" ; "-ideslave" |] coq_in_read coq_out_write coq_out_write
  in
  let _ = Unix.close coq_in_read in
  let _ = Unix.close coq_out_write in
  Unix.set_nonblock coq_out_read ;
  Some { proc_id = pid
       ; stdin = coq_in_write
       ; stdout = coq_out_read
       }

let close cp : unit =
  Unix.close cp.stdin ;
  Unix.close cp.stdout ;
  Unix.kill cp.proc_id 9


type xml =
  TAG of Xmlm.tag * xml list
| CDATA of string

exception BadTag of string * xml

let read_parse_stanza (fd : Unix.file_descr) : xml =
  (** This is only going to work if there is nothing else on the input stream.
   ** Otherwise it is possible that we will consume extra data in a buffer that
   ** is thrown away.
   **)
  let elem tg children = TAG (tg,children) in
  let data s = CDATA s in
  let buf = String.create 1024 in
  let max = ref 0 in
  let cur = ref 0 in
  let rec read_fd () =
    try
      let _ =
	if !cur = !max then
	  let cnt = Unix.read fd buf 0 1024 in
	  max := cnt ;
	  cur := 0
      in
      assert (!cur < !max) ;
      let ch = String.get buf !cur in
      cur := !cur + 1 ;
      Char.code ch
    with
    | Unix.Unix_error (Unix.EAGAIN, _, _) ->
      read_fd ()
    | Unix.Unix_error (err, s, _) ->
      Printf.fprintf stderr "%s: %s\r\n" (Unix.error_message err) s ;
      assert false
  in
  let input = Xmlm.make_input (`Fun read_fd) in
  let (_,tr) = Xmlm.input_doc_tree ~el:elem ~data:data input in
  tr

let rec get_attribute (a : string) (attrs : Xmlm.attribute list) =
  match attrs with
    [] -> None
  | ((_,attr),value) :: attrs ->
    if attr = a then Some value else get_attribute a attrs

let interp (goal : int option) (cmd : string) (cp : coq_process) =
  let cmd =
    match goal with
      None ->
	Printf.sprintf "<call val=\"interp\" id=\"0\">%s.</call>" cmd
    | Some goal ->
	Printf.sprintf "<call val=\"interp\" id=\"0\">%d: %s.</call>" goal cmd
  in
  let cmd_len = String.length cmd in
  let chars = Unix.write cp.stdin cmd 0 cmd_len in
  if chars = cmd_len then
    let out_xml = read_parse_stanza cp.stdout in
    match out_xml with
      TAG (((_ns,tg),attrs),_children) ->
	assert (tg = "value") ;
	if get_attribute "val" attrs = Some "good" then
	  Some cp
	else
	  None
    | _ ->
      assert false
  else
    None

let untag ?asrt tg =
  match tg with
    TAG (((_ns,tag_name),_attrs),c) ->
      let _ =
	match asrt with
	  Some e_tag_name ->
	    if tag_name <> e_tag_name then
	      raise (BadTag(Printf.sprintf "Expected '%s', got '%s'" e_tag_name tag_name, tg))
	| None -> ()
      in c
  | _ ->
    let msg =
      match asrt with
	Some e_tag_name ->
	  Printf.sprintf "Exptected '%s', got non-tag" e_tag_name
      | None -> "Expected any tag, got non-tag"
    in
    raise (BadTag(msg, tg))

let untag1 ?asrt tg =
  match untag ?asrt:asrt tg with
    [c] -> c
  | ls ->
    let len = List.length ls in
    raise (BadTag(Printf.sprintf "Expecting exactly one child, got %d" len, tg))

let untag_string tg =
  match untag ~asrt:"string" tg with
    CDATA s :: [] -> s
  | _ -> assert false

let parse_goal xml =
  match xml with
    TAG (((_ns,tg),attrs),_id :: hyps :: goal :: []) ->
      assert (tg = "goal") ;
      let hyps = List.map untag_string (untag ~asrt:"list" hyps) in
      let goal = untag_string goal in
      { premises = hyps
      ; goal = goal }
  | _ -> assert false

let rec print_tag out tg =
  match tg with
    TAG (((_,tg),_), ch) ->
      Printf.fprintf out "<%s>%a</%s>" tg (print_all print_tag) ch tg
  | CDATA c ->
    Printf.fprintf out "CDATA:%s" c
and print_all pr out ls =
  List.iter (Printf.fprintf out "%a" pr) ls

let all_goals cp =
  let cmd = Printf.sprintf "<call val=\"goal\" id=\"0\" />" in
  let cmd_len = String.length cmd in
  let chars = Unix.write cp.stdin cmd 0 cmd_len in
  if chars = cmd_len then
    let out_xml = read_parse_stanza cp.stdout in
(* DEBUG
    let _ = Printf.fprintf stderr "%a\n" print_tag out_xml in
*)
    let out_xml = untag1 ~asrt:"value" out_xml in
    let out_xml = untag ~asrt:"option" out_xml in
    match out_xml with
      [] -> [] (** NOTE: This means that I'm not in proof mode **)
    | [out_xml] ->
      let (out_xml::_) = untag ~asrt:"goals" out_xml in
      let goals = untag ~asrt:"list" out_xml in
      List.map parse_goal goals
    | _ -> assert false (** TODO: temporary **)
  else
    assert false
