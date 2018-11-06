let%expect_test _ =
  print_endline "Hello, world!";
  [%expect {| Hello, world! |}]
