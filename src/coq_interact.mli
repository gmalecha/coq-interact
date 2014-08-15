type coq_process

type runner = int option -> string -> coq_process -> coq_process option

val init : unit -> coq_process option

val close : coq_process -> unit

val interp : runner

(*
val focus : int -> runner -> runner
*)
