open! Base
open Stdio

module C = Configurator.V1

let log fmt = Printf.ksprintf (fun msg -> printf "discover: %s\n" msg) fmt

let discover_bin cfg path : string option =
  match C.which cfg path with
  | None ->
    log {|"%s" is not available|} path;
    None
  | Some _ -> Some path
;;

let discover_env var ~default : string =
  match Sys.getenv var with
  | None ->
    log {|"%s" env variable is not present. falling back to "%s"|} var default;
    default
  | Some x -> x
;;

type defaults =
  { cc : string
  ; cflags : string
  ; as_ : string
  ; as_flags : string
  ; ld : string
  ; ld_flags : string
  ; run : string
  ; run_flags : string
  }

type toolchain =
  { compile : string option
  ; assemble : string option
  ; link : string option
  ; run : string option
  }

let spf = Printf.sprintf

let discover_toolchain cfg defaults ~suffix =
  let suffix = String.uppercase suffix in
  let append bin flags = String.concat ~sep:" " [ bin; flags ] in

  log "discovering %s compiler" suffix;
  let compile =
    discover_env (spf "CC_%s" suffix) ~default:defaults.cc
    |> discover_bin cfg
    |> Option.map ~f:(fun bin ->
      let flags = discover_env (spf "CFLAGS_%s" suffix) ~default:defaults.cflags in
      append bin flags)
  in
  log "";

  log "discovering %s assembler" suffix;
  let assemble =
    discover_env (spf "AS_%s" suffix) ~default:defaults.as_
    |> discover_bin cfg
    |> Option.map ~f:(fun bin ->
      let flags = discover_env (spf "AS_FLAGS_%s" suffix) ~default:defaults.as_flags in
      append bin flags)
  in
  log "";

  log "discovering %s linker" suffix;
  let link =
    discover_env (spf "LD_%s" suffix) ~default:defaults.ld
    |> discover_bin cfg
    |> Option.map ~f:(fun bin ->
      let flags = discover_env (spf "LD_FLAGS_%s" suffix) ~default:defaults.ld_flags in
      append bin flags)
  in
  log "";

  log "discovering %s runner" suffix;
  let run =
    discover_env (spf "RUN_%s" suffix) ~default:defaults.run
    |> discover_bin cfg
    |> Option.map ~f:(fun bin ->
      let flags = discover_env (spf "RUN_FLAGS_%s" suffix) ~default:defaults.run_flags in
      append bin flags)
  in

  { compile; assemble; link; run }
;;

let export_toolchain toolchain ~suffix =
  let default = "none" in

  let compile = Option.value toolchain.compile ~default in
  Out_channel.write_all (spf "cc_%s" suffix) ~data:compile;

  let assemble = Option.value toolchain.assemble ~default in
  Out_channel.write_all (spf "as_%s" suffix) ~data:assemble;

  let link = Option.value toolchain.link ~default in
  Out_channel.write_all (spf "ld_%s" suffix) ~data:link;

  let run = Option.value toolchain.run ~default in
  Out_channel.write_all (spf "run_%s" suffix) ~data:run
;;

let () =
  let cfg = C.create "rukaml" in

  let gcc_amd64 = "x86_64-unknown-linux-gnu-gcc" in
  let defaults_amd64 =
    { cc = gcc_amd64
    ; cflags = "-g -fPIC -Wall -Wpedantic"
    ; as_ = "nasm"
    ; as_flags = "-f elf64"
    ; ld = gcc_amd64
    ; ld_flags = ""
    ; run = "qemu-x86_64"
    ; run_flags = "-L /opt/amd64/sysroot"
    }
  in
  let toolchain_amd64 = discover_toolchain cfg defaults_amd64 ~suffix:"amd64" in
  log "";

  let gcc_rv64 = "riscv64-unknown-linux-gnu-gcc" in
  let defaults_rv64 =
    { defaults_amd64 with
      cc = gcc_rv64
    ; as_ = gcc_rv64
    ; as_flags = "-x assembler -c"
    ; ld = gcc_rv64
    ; run = "qemu-riscv64"
    ; run_flags = "-L /opt/rv64/sysroot"
    }
  in
  let toolchain_rv64 = discover_toolchain cfg defaults_rv64 ~suffix:"rv64" in

  export_toolchain toolchain_amd64 ~suffix:"amd64";
  export_toolchain toolchain_rv64 ~suffix:"rv64"
;;
