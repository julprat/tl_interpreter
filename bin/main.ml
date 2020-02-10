open Tl_interpreter_lib

open Micro_proto

(* The tradeline contract *)

module type Tradeline_initial_parameters =
sig
  val owner : Address.t
end

module Tradeline_contract(X : Tradeline_initial_parameters) : Contract.S =
struct
  type storage = Mother.t

  (* Initial state *)
  let state = assert false

  let show = assert false

  let dispatch ~env ~entrypoint ~sender ~state ~arg ~amount =
    let open State in
    ignore env ;
    ignore sender ;
    ignore amount ;
    let emitted_operation_list = [] in
    match entrypoint with
    | "new" ->
      (match arg with
       | Value.Int tl_id ->
         return (emitted_operation_list, Mother.new_tl state tl_id)
       | _ ->
         user_error "ne: ill-typed argument")
    | "exec" ->
      (* TODO *)
      assert false
    | _ ->
      assert false
end

let make_tl_contract addr =
  let module P = struct let owner = addr end in
  (module (Tradeline_contract(P)) : Contract.S)

(* Example execution *)

let chain =
  let open Chain in
  let account0, account0_addr =
    Account.make (), Address.of_string "account0" in
  let tl_contract, tl_contract_addr =
    make_tl_contract account0_addr, Address.of_string "tl_contract" in

  (* simulated execution *)
  let open State in
  run begin
    let* () = originate account0_addr (account0, Mutez.of_int_exn 100) in
    let* () = originate tl_contract_addr (tl_contract, Mutez.of_int_exn 0) in
    let* () = State.sleep (Time.span 10) in
    return ()
  end


let () = print_string "ok\n"
