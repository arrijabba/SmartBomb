
(* type transfer_FA12 = (address, "from", (address, "to", nat, "value") michelson_pair, "") michelson_pair *)

type balance_of_param_FA12 = (address * nat contract)

type transfer_FA12 = [@layout:comb] {
  from : address;
  [@annot:to] to_ : address;
  value : nat;
}

(* type balance_of_param_FA12 = [@layout:comb] {
  owner : address;
  balance_callback : nat contract;
}
 *)

type fa2_token = [@layout:comb] {
    token_id : token_id;
    amount : nat;
}

type tokens = [@layout:comb] {
    fa2_address : address;
    fa2_batch : (fa2_token list);
}

type single_tokens = [@layout:comb] {
    fa2_address : address;
    fa2_token : fa2_token;
}

type fa_currency = [@layout:comb] {
    fa2_address : address;
    token_id : token_id option;
}

let get_token_id (token : fa_currency) : nat =
  match (token.token_id) with
      None -> (failwith "NO_ID" : nat)
    | Some f -> f
(* ----------------------- FA2  -------------------------- *)
(* ----------------------- FA2  -------------------------- *)
(* ----------------------- FA2  -------------------------- *)
(* ----------------------- FA2  -------------------------- *)
(* ----------------------- FA2  -------------------------- *)
(* ----------------------- FA2  -------------------------- *)
let address_to_contract_balance_entrypoint (add : address) : (balance_of_param contract) =
  let c : balance_of_param contract option = Tezos.get_entrypoint_opt "%balance_of" add in
  match c with
    None -> (failwith "Invalid FA2 Address" : balance_of_param contract)
  | Some c ->  c


let address_to_contract_transfer_entrypoint (add : address) : ((transfer list) contract) =
  let c : (transfer list) contract option = Tezos.get_entrypoint_opt "%transfer" add in
  match c with
    None -> (failwith "Invalid FA2 Address" : (transfer list) contract)
  | Some c ->  c

let transfer_tokens_in_single_contract (from_ : address) (to_ : address) (tokens : single_tokens) : operation = 

  let to_tx : transfer_destination = {
      to_ = to_;
      token_id = tokens.fa2_token.token_id;
      amount = tokens.fa2_token.amount;
   } in
   let transfer_param = [{from_ = from_; txs = [to_tx]}] in
   let c = address_to_contract_transfer_entrypoint(tokens.fa2_address) in

   (Tezos.transaction transfer_param 0mutez c) 

let fa2_transfer (from_, to_, qty, currency : address * address * nat * fa_currency) : operation = 
  let token_id = get_token_id(currency) in
  let tx_param : single_tokens = 
    ({
        fa2_address = currency.fa2_address;
        fa2_token = ({token_id = token_id; amount = qty} : fa2_token)
    }) in
  let op = transfer_tokens_in_single_contract from_ to_ tx_param in
  op


let fa2_upd_op (from_, to_, qty, currency : address * address * nat * fa_currency) : operation = begin

  let token_id : nat = match currency.token_id with
    Some id -> id
  | None -> (failwith "NO_ID" : nat) in

  let upd_op_inner : operator_param = {
    owner= from_;
    operator = to_;
    token_id = token_id;
  } in

  let upd_op_param : update_operator = if(qty > 0n) then Add_operator(upd_op_inner) else Remove_operator(upd_op_inner) in

  let upd_op : (update_operator list) contract option = Tezos.get_entrypoint_opt "%update_operators" currency.fa2_address in

  match upd_op with
    None -> (failwith "FA2_UPD_OP_FAIL" : operation)
  | Some c -> Tezos.transaction [upd_op_param] 0mutez c
end

(* ----------------------- END FA2  -------------------------- *)
(* ----------------------- END FA2  -------------------------- *)
(* ----------------------- END FA2  -------------------------- *)
(* ----------------------- END FA2  -------------------------- *)
(* ----------------------- END FA2  -------------------------- *)








(* ----------------------- FA1.2  -------------------------- *)
(* ----------------------- FA1.2  -------------------------- *)
(* ----------------------- FA1.2  -------------------------- *)
(* ----------------------- FA1.2  -------------------------- *)
(* ----------------------- FA1.2  -------------------------- *)
(* ----------------------- FA1.2  -------------------------- *)
let address_to_contract_balance_entrypoint_FA12 (add : address) : (balance_of_param_FA12 contract) =
  let c : balance_of_param_FA12 contract option = Tezos.get_entrypoint_opt "%getBalance" add in
  match c with
    None -> (failwith "Invalid FA1.2 Address" : balance_of_param_FA12 contract)
  | Some c ->  c

let address_to_contract_transfer_entrypoint_FA12 (add : address) : (transfer_FA12 contract) =
  let c : transfer_FA12 contract option = Tezos.get_entrypoint_opt "%transfer" add in
  match c with
    None -> (failwith "Invalid FA1.2 Address" : transfer_FA12 contract)
  | Some c ->  c

let get_self_bal (add : address) : ((balance_of_response list) contract) = 
  let c : (balance_of_response list) contract option = Tezos.get_entrypoint_opt "%flash_S2Q_Reciever_Balance" add in
  match c with
    None -> (failwith "Invalid Address" : (balance_of_response list) contract)
  | Some c ->  c

let fa12_transfer (from_, to_, qty, currency : address * address * nat * fa_currency) : operation = 
    let transfer_param : transfer_FA12 = {
      from = from_;
      to_ = to_;
      value = qty;
    } in
    let c = address_to_contract_transfer_entrypoint_FA12(currency.fa2_address) in
    Tezos.transaction transfer_param 0mutez c


type approve_params = [@layout:comb] {
  spender: address;
  value: nat
}

let fa12_approve (from_, to_, qty, currency : address * address * nat * fa_currency) : operation = begin

  let a_p : approve_params = {
    spender = to_;
    value = qty
  } in

  let approve : approve_params contract option = Tezos.get_entrypoint_opt "%approve" currency.fa2_address in

  match approve with
    None -> (failwith "FA12_APPROVE_FAIL" : operation)
  | Some c -> Tezos.transaction a_p 0mutez c

(* ----------------------- END FA1.2  -------------------------- *)
(* ----------------------- END FA1.2  -------------------------- *)
(* ----------------------- END FA1.2  -------------------------- *)
(* ----------------------- END FA1.2  -------------------------- *)
(* ----------------------- END FA1.2  -------------------------- *)
(* ----------------------- END FA1.2  -------------------------- *)
end

let fa_transfer (from_, to_, qty, currency : address * address * nat * fa_currency) : operation =
  match currency.token_id with
    Some _ -> fa2_transfer(from_, to_, qty, currency)
  | None -> fa12_transfer(from_, to_, qty, currency)

let fa_approve (from_, to_, qty, currency : address * address * nat * fa_currency) : operation =
  match currency.token_id with
    Some _ -> fa2_upd_op(from_, to_, qty, currency)
  | None -> fa12_approve(from_, to_, qty, currency)