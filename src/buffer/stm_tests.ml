open QCheck
open STM
open Util

(** parallel STM tests of Buffer *)

(* port from the QCSTM example *)
module BConf =
struct
  type cmd =
    | Contents
    | To_bytes
    | Sub of (int * int)
    (* Blit *)
    | Nth of int
    | Length
    | Clear
    | Reset
    | Add_char of char
    (* Add_utf8_uchar | Add_utf_16le_uchar | Add_utf_16be_uchar *)
    | Add_string of string
    | Add_bytes of bytes
    | Truncate of int
  [@@deriving show { with_path = false }]

  type state = char list (* in reverse *)
  type sut = Buffer.t

  let arb_cmd s =
    let int_gen,string_gen = Gen.(small_nat,small_string) in
    QCheck.make ~print:show_cmd
      Gen.(oneof [return Contents;
                  return To_bytes;
                  map2 (fun off len -> Sub (off, len)) int_gen int_gen;
                  map (fun i -> Nth i) int_gen;
                  return Length;
                  return Clear;
                  return Reset;
                  map (fun c -> Add_char c) char;
                  map (fun s -> Add_string s) string_gen;
                  map (fun b -> Add_bytes (String.to_bytes b)) string_gen;
                  map (fun i -> Truncate i) (let len = List.length s in
                                                 if len = 0
                                                 then return 0
                                                 else int_bound (len - 1));
                 ])

  let init_state  = []

  let rev_explode s =
    let chars = ref [] in
    String.iter (fun c -> chars := c::!chars) s;
    !chars

  let explode s = List.rev (rev_explode s)
  let to_string s = List.rev s
                    |> List.map (fun c -> Printf.sprintf "%c" c)
                    |> String.concat ""

  let next_state c s = match c with
    | Contents -> s
    | To_bytes -> s
    | Sub _ -> s (* sub returns a copy *)
    | Nth _ -> s
    | Length -> s
    | Clear -> []
    | Reset -> []
    | Add_char ch -> ch::s
    | Add_string str -> (rev_explode str)@s (*s@(explode str)*)
    | Add_bytes bytes -> (rev_explode (String.of_bytes bytes))@s
    | Truncate i ->
      let rec trunc buf n = match buf,n with
        | [],0 -> []
        | [],_ -> []
        | _::_,0 -> []
        | c::cs,_ -> c::trunc cs (n-1) in
      List.rev (trunc (List.rev s) i)

  let init_sut () = Buffer.create 16
  let cleanup b   = Buffer.reset b

  let precond c s = match c with
    | Truncate i -> i >= 0 && i <= List.length s
    | _ -> true

  let run c b = match c with
    | Contents        -> Res (string, Buffer.contents b)
    | To_bytes        -> Res (bytes,  Buffer.to_bytes b)
    | Sub (off, len)  -> Res (result string exn, protect (Buffer.sub b off) len)
    | Nth i           -> Res (result char exn, protect (Buffer.nth b) i)
    | Length          -> Res (int, Buffer.length b)
    | Clear           -> Res (unit, Buffer.clear b)
    | Reset           -> Res (unit, Buffer.reset b)
    | Add_char ch     -> Res (unit, Buffer.add_char b ch)
    | Add_string str  -> Res (unit, Buffer.add_string b str)
    | Add_bytes bytes -> Res (unit, Buffer.add_bytes b bytes)
    | Truncate i      -> Res (result unit exn, protect (Buffer.truncate b) i)

  let postcond c s res = match c, res with
    | Contents, Res ((String,_),str)   -> explode str = List.rev s
    | To_bytes, Res ((Bytes,_), bytes) -> bytes = Bytes.of_string (to_string s)
    | Sub (off, len), Res ((Result (String,Exn),_), str) ->
       if off > List.length s || off + len > List.length s
       then str = Error (Invalid_argument "Buffer.sub")
       else str = Ok (String.sub (to_string s) off len)
    | Nth i, Res ((Result (Char,Exn),_), r) ->
       if i < 0 || i >= List.length s
       then r = Error (Invalid_argument "Buffer.nth")
       else r = Ok (List.nth (List.rev s) i)
    | Length, Res ((Int,_),i) -> i = List.length s
    | Clear, Res ((Unit,_),_) -> true
    | Reset, Res ((Unit,_),_) -> true
    | Add_char _, Res ((Unit,_),_)   -> true
    | Add_string _, Res ((Unit,_),_) -> true
    | Add_bytes _, Res ((Unit,_),_)  -> true
    | Truncate i, Res ((Result (Unit,Exn),_),r) ->
       if i < 0 || i > List.length s
       then r = Error (Invalid_argument "Buffer.truncate")
       else r = Ok ()
    | _, _ -> false
end

module BufferSTM = STM.Make(BConf)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 100 in
   [BufferSTM.agree_test     ~count ~name:"buffer test";         (* this test is expected to succeed *)
    BufferSTM.agree_test_par ~count ~name:"buffer test parallel" (* this test is expected to fail *)])
