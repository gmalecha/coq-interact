type coq_process =
{ proc_id : int
; stdin : Unix.file_descr
; stdout : Unix.file_descr
}

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

(*
let end_stanza = "</value>"

let buffer_ends_with (b : Buffer.t) (s : string) : bool =
  let buf_len = Buffer.length buf in
  if buf_len < String.length end_stanza then
    false
  else
    Buffer.sub buf (buf_len - end_stanza) end_stanza = end_stanza
*)
