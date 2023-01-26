let yojson =
  (module Yojson.Basic : Alcotest.TESTABLE with type t = Yojson.Basic.t)

let test_query schema ctx query expected =
  match Graphql_parser.parse query with
  | Error err -> failwith err
  | Ok doc ->
      (Graphql_eio.Schema.execute schema ctx doc |> function
       | Ok (`Response data) -> data
       | Ok (`Stream (lst, _)) ->
           `List
             (List.fold_right
                (fun x acc -> match x with Ok data -> data :: acc | _ -> acc)
                lst [])
       | Error err -> err)
      |> fun result ->
      Alcotest.check yojson "invalid execution result" expected result

let schema =
  Graphql_eio.Schema.(
    schema
      [
        field "direct_string" ~typ:(non_null string)
          ~args:Arg.[]
          ~resolve:(fun _ () -> "foo");
        io_field "io_int" ~typ:(non_null int)
          ~args:Arg.[]
          ~resolve:(fun _ () -> Ok 42);
      ]
      ~subscriptions:
        [
          subscription_field "int_stream" ~typ:(non_null int)
            ~args:Arg.[]
            ~resolve:(fun _ ->
              let stream = [ 1; 2; 3 ] in
              let destroy () = () in
              Ok (stream, destroy));
        ])

let suite =
  [
    ( "execution",
      `Quick,
      fun () ->
        test_query schema () "{ direct_string io_int  }"
          (`Assoc
            [
              ( "data",
                `Assoc [ ("direct_string", `String "foo"); ("io_int", `Int 42) ]
              );
            ]) );
    ( "subscription",
      `Quick,
      fun () ->
        test_query schema () "subscription { int_stream }"
          (`List
            [
              `Assoc [ ("data", `Assoc [ ("int_stream", `Int 1) ]) ];
              `Assoc [ ("data", `Assoc [ ("int_stream", `Int 2) ]) ];
              `Assoc [ ("data", `Assoc [ ("int_stream", `Int 3) ]) ];
            ]) );
  ]

let () = Alcotest.run "graphql-server" [ ("eio", suite) ]
