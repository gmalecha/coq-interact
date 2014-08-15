type coq_process

type coq_string = string

type goal =
{ premises : coq_string list
; goal : coq_string
}

val print_goal : out_channel -> goal -> unit

type runner = int option -> string -> coq_process -> coq_process option

val init : unit -> coq_process option

val close : coq_process -> unit

val interp : runner

val all_goals : coq_process -> goal list

(*
val focus : int -> runner -> runner
*)
