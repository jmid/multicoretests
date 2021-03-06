open Lin_tests_common

(** This is a driver of the negative tests over the Effect module *)

(* Q: What constitutes a Fiber-unsafe API?
   A: Tests that behave differently when run with/without a fiber-based scheduler certainly do.
   The following raises the Yield effect inside the `run` command.
   This results in an `Unhandled` exception when running outside a fiber-based scheduler,
   such as when interpreting these sequentially. *)
module RT_int' =
  Lin.Make(struct
    include RConf_int
    let run c r = match c with
      | Add i -> (let tmp = Sut_int.get r in Lin.yield (); Sut_int.set r (tmp+i); RAdd)
      | _     -> run c r
  end)

module RT_int64' =
  Lin.Make(struct
    include RConf_int64
    let run c r = match c with
      | Add i -> (let tmp = Sut_int.get r in Lin.yield (); Sut_int.set r (Int64.add tmp i); RAdd)
      | _     -> run c r
  end)

module CLT_int' =
  Lin.Make(struct
    include CLConf(Int)
    let run c r = match c with
      | Add_node _ -> Lin.yield (); run c r
      | Member _   -> run c r
  end)

module CLT_int64' =
  Lin.Make(struct
    include CLConf(Int64)
    let run c r = match c with
      | Add_node _ -> Lin.yield (); run c r
      | Member _   -> run c r
  end)

;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 20_000 in [
      (* We don't expect the first four tests to fail as each `cmd` is completed before a `Yield` *)
      RT_int.lin_test     `Effect ~count ~name:"ref int test";
      RT_int64.lin_test   `Effect ~count ~name:"ref int64 test";
      CLT_int.lin_test    `Effect ~count ~name:"CList int test";
      CLT_int64.lin_test  `Effect ~count ~name:"CList int64 test";
      (* These next four tests are negative - and are expected to fail with exception `Unhandled` *)
      RT_int'.lin_test    `Effect ~count ~name:"negative ref int test";
      RT_int64'.lin_test  `Effect ~count ~name:"negative ref int64 test";
      CLT_int'.lin_test   `Effect ~count ~name:"negative CList int test";
      CLT_int64'.lin_test `Effect ~count ~name:"negative CList int64 test"
    ])
