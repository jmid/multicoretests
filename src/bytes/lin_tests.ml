
module Bytes_test = struct
type t = Bytes.t

(* external get : bytes -> int -> char = "get_from_runtime" *)

let get = Bytes.get
 
end

module TestConf = struct
type t = Bytes_test.t

let init () = Stdlib.Bytes.make 42 '0'
let cleanup _ = ()

open Lin_api
let api =
  [ val_ "Bytes_test.get" Bytes_test.get (t @-> int @-> returning_or_exc char)
]

end

module BConf = struct (*this works and passes all of them ... perhaps because
                      lin never actually indexes into the bytes?*)
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin_api
  let api =
    [
      val_ "Bytes.sub_string" Bytes.sub_string (t @-> int @-> int @-> returning_or_exc string);
      val_ "Bytes.length" Bytes.length (t @-> returning int);
      val_ "Bytes.fill" Bytes.fill (t @-> int @-> int @-> char @-> returning_or_exc unit);
      val_ "Bytes.blit_string" Bytes.blit_string
        (string @-> int @-> t @-> int @-> int @-> returning_or_exc unit);
      val_ "Bytes.index_from" Bytes.index_from (t @-> int @-> char @-> returning_or_exc int)]
end

module BConfGet = struct (*this does not work*)
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin_api
  let api =
    [
      val_ "Bytes.unsafe_get" Bytes.unsafe_get (t @-> int @-> returning_or_exc char)
    ]
end


module BConfSet = struct (*this does not work*)
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin_api
  let api =
    [
      val_ "Bytes.set" Bytes.set
        (t @-> int @-> char @-> returning_or_exc unit)
    ]
end


module BT = Lin_api.Make(TestConf)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  BT.lin_test `Domain ~count:1000 ~name:"Byte test with domains";
  BT.lin_test `Thread ~count:1000 ~name:"Byte test with threads";
]
