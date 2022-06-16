open QCheck

type 'a ty = ..
type 'a ty +=
  | Unit : unit ty
  | Bool : bool ty
  | Char : char ty
  | Int : int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | String : string ty
  | Bytes : bytes ty
  | Exn : exn ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty

type 'a ty_show = 'a ty * ('a -> string)

val unit   : unit ty_show
val bool   : bool ty_show
val char   : char ty_show
val int    : int ty_show
val int32  : int32 ty_show
val int64  : int64 ty_show
val float  : float ty_show
val string : string ty_show
val bytes  : bytes ty_show
val option : 'a ty_show -> 'a option ty_show
val exn    : exn ty_show
val result : 'a ty_show -> 'b ty_show -> ('a, 'b) result ty_show
val list   : 'a ty_show -> 'a list ty_show

type res = Res : 'a ty_show * 'a -> res

val show_res : res -> string

val repeat : int -> ('a -> bool) -> 'a -> bool

(** [protect f a] applies [f] to [a] and wraps the result of the computation in a [result] type,
    catching the possible excpetion in the process *)
val protect : ('a -> 'b) -> 'a -> ('b, exn) result

module type Spec =
sig
  type cmd
  (** The type of commands *)

  type state
  (** The type of the model's state *)

  type sut
  (** The type of the system under test *)

  val arb_cmd : state -> cmd arbitrary
  (** A command generator. Accepts a state parameter to enable state-dependent [cmd] generation. *)

  val init_state : state
  (** The model's initial state. *)

  val next_state : cmd -> state -> state
  (** Move the internal state machine to the next state. *)

  val init_sut : unit -> sut
  (** Initialize the system under test. *)

  val cleanup : sut -> unit
  (** Utility function to clean up the [sut] after each test instance,
      e.g., for closing sockets, files, or resetting global parameters*)

  val precond : cmd -> state -> bool
  (** [precond c s] expresses preconditions for command [c].
      This is useful, e.g., to prevent the shrinker from breaking invariants when minimizing
      counterexamples. *)

  val show_cmd : cmd -> string
  (** [show_cmd c] returns a string representing the command [c]. *)

  val run : cmd -> sut -> res
  (** [run c i] should interpret the command [c] over the system under test (typically side-effecting). *)

  val postcond : cmd -> state -> res -> bool
  (** [postcond c s res] checks whether [res] arising from interpreting the
      command [c] over the system under test with [run] agrees with the
      model's result.
      Note: [s] is in this case the model's state prior to command execution. *)
end

module AddGC : functor (Spec: Spec) -> Spec
module STM_Seq : sig
  module Make : functor (Spec: Spec) -> sig
                  val cmds_ok: Spec.state -> Spec.cmd list -> bool
                  val interp_sut_res: Spec.sut -> Spec.cmd list -> (Spec.cmd * res) list
                  val check_obs: (Spec.cmd * res) list ->
                                 (Spec.cmd * res) list ->
                                 (Spec.cmd * res) list ->
                                 Spec.state -> bool
                  val gen_cmds_size: Spec.state -> int Gen.t -> Spec.cmd list Gen.t
                  val agree_prop: Spec.cmd list -> bool
                  val agree_test: count:int -> name:string -> Test.t
                end
end

module STM_Domain : sig
  module Make : functor (Spec: Spec) -> sig
                  val agree_prop_par: (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
                  val agree_test_par: count:int -> name:string -> Test.t
                end
end

module STM_Thread : sig
  module Make : functor (Spec: Spec) -> sig
                  val agree_prop_conc: (Spec.cmd list * Spec.cmd list * Spec.cmd list) -> bool
                  val agree_test_conc: count:int -> name:string -> Test.t
                end
end
