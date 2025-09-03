open! Base
open Stdio

module TestSpec = struct
  type target =
    | Amd64
    | Rv64
  [@@deriving sexp, show { with_path = false }]

  type run_spec =
    { stdout : string [@default ""]
    ; exit : int [@default 0]
    }
  [@@deriving sexp, show { with_path = false }]

  type t =
    { targets : target list
    ; run : run_spec option [@sexp.option]
    }
  [@@deriving sexp, show { with_path = false }]

  let of_file ~path =
    let ( let* ), return, fail = Result.( >>= ), Result.return, Result.fail in
    let rec parse_next parse file =
      let* line = In_channel.input_line file |> Result.of_option ~error:"eof" in
      match parse (`String line) with
      | Angstrom.Buffered.Done (_, res) -> return res
      | Fail (_, _, msg) -> fail msg
      | Partial f -> parse_next f file
    in

    let pcomment =
      let open Angstrom in
      skip_while Char.is_whitespace *> string "(*" *> many_till any_char (string "*)")
      >>| String.of_char_list
    in

    let parse = Angstrom.Buffered.feed @@ Angstrom.Buffered.parse pcomment in
    let* comment = In_channel.with_file path ~f:(parse_next parse) in

    let* sexp =
      match Parsexp.Many.parse_string comment with
      | Error err -> fail (Parsexp.Parse_error.message err)
      | Ok [] -> fail "test spec not found"
      | Ok (hd :: tl) ->
        if Sexp.equal hd (Atom "test")
        then return (Sexp.List tl)
        else fail "header 'test' required"
    in
    try return (t_of_sexp sexp) with
    | Sexplib.Conv_error.Of_sexp_error (Failure msg, _) -> fail msg
  ;;
end

module Test = struct
  type t =
    { path : File_path.Relative.t
    ; spec : TestSpec.t
    }
  [@@deriving show { with_path = false }]

  let compile = ()
  let assemble = ()
  let link = ()
  let run = ()
end

let () = Sys.get_argv () |> Array.iter ~f:print_endline

(* let () = *)
(*   TestSpec.of_file (File_path.of_string "fac.ml") *)
(*   |> Result.ok_or_failwith *)
(*   |> TestSpec.pp Stdlib.Format.std_formatter *)
(* ;; *)

(* ===================================================================== *)

let () =
  Out_channel.write_all
    "expected.dune"
    ~data:
      {|
(rule
 (target fac.rv64.out)
 (deps ../tests/fac.ml)
 (mode promote)
 (action
  (run %{project_root}/back/rv64/rv64_compiler.exe %{deps} -o %{target})))|}
;;

let () =
  Out_channel.write_all
    "artifacts.dune"
    ~data:
      {|
(rule
  (target fac.rv64.o)
  (deps ../expected/fac.rv64.out)
  (mode (promote (until-clean) (into obj)))
  (action (run mocc %{deps} -c -o %{target})))

(rule
  (target fac.rv64.exe)
  (deps fac.rv64.o)
  (mode (promote (until-clean) (into exe)))
  (action (run mocc %{deps} -o %{target})))

(rule
  (mode (promote (until-clean) (into cram)))
  (action (with-stdout-to fac.rv64.t (echo "  $ ../fac.rv64.exe\n  Hello World!\n"))))
|}
;;

let () =
  Out_channel.write_all
    "cram.dune"
    ~data:
      {|
(cram
  (applies_to fac.rv64)
  (deps ../fac.rv64.exe))
|}
;;
