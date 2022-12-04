#include "./fa2/fa2_interface.mligo"
#include "./fa2/fa2_multi_ft_token_manager_casino.mligo"
#include "fa_currency_lib.mligo"

(* I understand that timelock may be deprecated, but at some point im sure itll be back. *)

(* -- *)

(* 1. Organizer uses %Initialize to set the total amount of tez, and amount thats open to insurance. Chest should be set to open in 1 week. *)

(* 2. Users verify the chest matches the explosion time in contract. Users use %Insure to post some tez and mint Insurance Pool (IP) Tokens *)

(* 3. Insurance period completes and game is played off-chain, elsewhere *)

(* 4. Organizer posts the confirmed winners list and refutation is open *)

(* 5.A If users refute above 70-80%, then the timelock will explode (5% tax taken from organizer, no prize payouts) upon opening. Users can solve the timelock if the organizer runs away. *)

(* 5.B If users say nothing, then the timelock will defuse on unlock (1% tax, payouts as posted) upon opening. Users can still solve the timelock if the organizer runs away. *)

(* We use timelock here to make sure the insurers can act on the bomb if organizer goes absent. *)

(* There is a large incentive for players of the game to provide insurance. A TODO would be to give a confirmed list of players first access to insurance. *)


type initialize_game = [@layout:comb] {
    payout : nat list;
    bomb : bytes;
}

type escrow_stage =
  | Pregame
  | Initialized
  | Posted
  | Exploded

type storage = {
    
    assets : multi_ft_token_storage;

    accuracy : nat;

    stage : escrow_stage;

    stage_timestamp : timestamp;
    
    (* Locks funds in to initialize the michelson timelock bomb. Also posts the winners list at the end. *)
    owner : address;

    (* Banned from using timelock in tezos client, so fallback to hashed secret :/ *)

    (* Built and uploaded by the organizer. It will be available to unlock after they  *)
    locked_bomb : bytes option; (* Partial key *)

    (* This goes to insurance providers on a successful run with secret posted. ~1% *)
    successful_defuse_tax : nat;

    (* This goes to insurance providers on a liquidation through accusation. ~5% *)
    explosion_tax : nat;



    (* Set on initialization *)
    total_prize : tez;

    (* should be a list of % values that add up to total_prize. *)
    prize_structure : nat list;

    (* The final posted results. Open to refutation by insurance providers once posted. *)
    results : address list option;

    (* Amount of time that refutation is open. Bomb cannot be called until results are posted and this time has elapsed. *)
    refutation_sec : nat;

    (* Keeps track of how much has been refuted. *)
    refutation_amt : nat;

    (* What is the cutoff for a successful refutation. ~70-80%. Set this to 80 for 80%, we just check the raw token amounts using accuracy. *)
    refutation_pct : nat;

    (* Only allow refutation once. *)
    refutors : address set;



    (* Users can lock in XTZ up to double the total prize, *)
    insurance_pool_size : tez;

    (* Users can lock in XTZ up to double the total prize, *)
    insurance_pool_limit : tez;

    (* When to stop accepting requests for insurance. *)
    insurance_pool_sec : nat;

    (* To make sure someone doesn't take the whole pool. *)
    (* In production you'd use some sort of trusted distribution of access to cut down on copywallets (doesnt have to be full KYC) *)
    max_insurance_stake_per_user : tez;
}

type return = operation list * storage

(* 


  _    _ _______ _____ _      
 | |  | |__   __|_   _| |     
 | |  | |  | |    | | | |     
 | |  | |  | |    | | | |     
 | |__| |  | |   _| |_| |____ 
  \____/   |_|  |_____|______|
                              
                              


*)

let validate_secret (sec_hash, revealed : bytes * bytes) : unit option = if Crypto.sha256 revealed <> sec_hash then (None : unit option) else (Some unit)

let is_pregame (s : escrow_stage) : bool = match s with
  | Pregame -> true
  | Initialized -> false
  | Posted -> false
  | Exploded -> false


let is_initialized (s : escrow_stage) : bool = match s with
  | Pregame -> false
  | Initialized -> true
  | Posted -> false
  | Exploded -> false


let is_posted (s : escrow_stage) : bool = match s with
  | Pregame -> false
  | Initialized -> false
  | Posted -> true
  | Exploded -> false


let is_exploded (s : escrow_stage) : bool = match s with
  | Pregame -> false
  | Initialized -> false
  | Posted -> false
  | Exploded -> true

let assert_msg (condition, msg : bool * string ) : unit =
  if (not condition) then failwith(msg) else unit

let get_balance_immediate (addy, ledger : address * ledger) : nat =
  let key = addy, 0n in
  let bal = get_balance_amt (key, ledger) in
  bal

let get_supply_immediate (supply : token_total_supply) : nat =
  let opt = Big_map.find_opt 0n supply in
  match opt with
  | None -> 0n
  | Some b -> b

let fail_if_not_sender (a : address) : unit =
  if Tezos.get_sender() <> a
  then failwith "INCORRECT_SENDER"
  else unit


let flush (amount, sink : tez * address) : operation = 
  let c : unit contract = match (Tezos.get_contract_opt sink : unit contract option) with
      Some contract -> contract
    | None -> (failwith "0" : unit contract)
  in
  (Tezos.transaction unit amount c)

(* Do payouts by recursion through the posted list of percentages. *)
let rec pay_out (payout_list, payout_pcts, total_prize, accuracy, self, ops: address list * nat list * nat * nat * address * operation list) : operation list = 
  
  (* when we reach the end, we end. *)
  match payout_list with
    | [] -> (ops)
    | user :: next_payouts -> begin

      match payout_pcts with
        | [] -> (failwith "LIST_MISMATCH" : operation list)
        | pct :: next_pcts -> begin

        let pay : nat = (total_prize * pct) / accuracy in

        let pay_amount : tez = pay * 1mutez in

        let pay_tez : operation = flush(pay_amount, user) in

        pay_out(next_payouts, next_pcts, total_prize, accuracy, self, pay_tez::ops) 
      end
    end
(* Initialize by storing the Bomb aka Timelock Chest aka partial encrypted key. *)
let init (self, init, storage : address * initialize_game * storage) : return = begin

    assert_msg (is_pregame(storage.stage) = true, "MUST_BE_PREGAME");
    
    let () = fail_if_not_sender(storage.owner) in

    let now : timestamp = Tezos.get_now() in
    let user : address = Tezos.get_sender() in

    (* Create should send this. *)
    let prize : tez = Tezos.get_balance() in

    (* Limit Should be 2x the prize. *)
    let insurance_pool_limit : tez = prize * 2n in

    (* Create Insurance Pool token *)
    let new_assets = create_token({
      token_id = 0n;
      token_info = Map.literal [
        "symbol", 0x53424950;
        "name", 0x536d617274426f6d6220496e737572616e636520506f6f6c20546f6b656e;
        "decimals", 0x32;
        "thumbnailUri", 0x697066733a2f2f516d50573739414161747a73325a3941734c72674a32796243585a53573857324d4444714e76433955364e6a524b;
      ]
    }, storage.assets) in

    let new_storage : storage = 
    { 
      storage with 

        stage = (Initialized : escrow_stage);
        stage_timestamp = now;

        assets = new_assets;
        
        (* Store chest. *)
        locked_bomb = (Some init.bomb);

        (* Payout will have to match this on post *)
        prize_structure = init.payout; 

        total_prize = prize; 
        insurance_pool_limit = insurance_pool_limit; 
    } in

    (([] : operation list), new_storage)
end

(* Initialize by storing the Bomb aka Timelock Chest aka partial encrypted key. *)
let insure (self, init, storage : address * tez * storage) : return = begin

    assert_msg (is_initialized(storage.stage) = true, "MUST_BE_INITIALIZED");

    let now : timestamp = Tezos.get_now() in
    let user : address  = Tezos.get_sender() in

    (* Ensure insurance phase is still open (init timestamp + seconds) *)
    assert_msg (now <= storage.stage_timestamp + int(storage.insurance_pool_sec), "INSURE_CLOSED");

    (* How much insurance user wants to provide. *)
    let requested : tez = Tezos.get_amount() in

    (* Ensure amount isn't too much *)
    assert_msg (requested <= storage.max_insurance_stake_per_user, "AMONUT_TOO_HIGH");

    (* Ensure adding the requested insurance doesn't pass limit. *)
    assert_msg (storage.insurance_pool_size + requested <= storage.insurance_pool_limit, "AMONUT_TOO_HIGH");

    (* All checks complete. Mint the user some tokens. *)
    (* Because we use a token/pool system instead of accounts, we don't really have to track individual stakes. Users can add twice with no problems. *)

    (* Again, there is high incentive for users shwo are actually playing to have some insurance in the pool. *)

    (* This is in mutez *)
    let amount : nat = requested / 1mutez in

    let limit  : nat = storage.insurance_pool_limit / 1mutez in

    (* If at the "Limit", there are 100% of tokens, just mint this amount of tokens to match. *)
    (* multiply by accuracy at the beginning of complex mathy things. *)
    (* For a limit of 100xtz, adding 45xtz would mint 450000 tokens (with 2 decimals added, 4500.00) *)
    let percentage_of_limit : nat = (amount * storage.accuracy) / limit in
    
    (* Mint tokens. *)
    let new_assets_mint = mint_tokens([{
      owner = user;
      token_id = 0n;
      amount = percentage_of_limit;
    }], storage.assets) in

    let new_storage : storage = { storage with assets = new_assets_mint; insurance_pool_size = storage.insurance_pool_size + requested; } in

    (([] : operation list), new_storage)
end


(* Simply store the potential winner list for insurer review. *)
let post (self, results, storage : address * address list * storage) : return = begin

    let () = fail_if_not_sender(storage.owner) in

    let now : timestamp = Tezos.get_now() in
    let user : address = Tezos.get_sender() in

    (* Ensure insurance phase is closed *)
    assert_msg (now > storage.stage_timestamp + int(storage.insurance_pool_sec), "INSURE_STILL_OPEN");

    assert_msg (List.length(results) = List.length(storage.prize_structure), "LIST_MISMATCH");

    let new_storage : storage = 
    { 
      storage with 

        stage = (Posted : escrow_stage);
        stage_timestamp = now;

        results = (Some results);
    } in

    (([] : operation list), new_storage)
end

(* Users can refute once with their whole balance. *) 
(* Refutation is never blocked, but if the insurers don't come to an accusation consensus by the time the refutation_sec is over, they are at risk of letting organizer defuse bomb. *)
let refute (self, storage : address * storage) : return = begin

    assert_msg (is_posted(storage.stage) = true, "MUST_BE_POSTED");

    let now : timestamp = Tezos.get_now() in
    let user : address = Tezos.get_sender() in

    (* Get full balance amount for adding to total. We do this because we don't want to transfer or burn their tokens. *)
    let refutation_from_user : nat = get_balance_immediate(user, storage.assets.ledger) in

    (* Log the new refutor *)
    let new_refutors : address set = if Set.mem user storage.refutors = true then (failwith "ALREADY_REFUTED" : address set ) else Set.add user storage.refutors in

    let new_storage : storage = 
    { 
      storage with 
        refutation_amt = storage.refutation_amt + refutation_from_user;
        refutors = new_refutors;
    } in

    (([] : operation list), new_storage)
end

(* ------------------------------------------------------ *)
(* Bomb has two paths, based on the amount of refutation  *)
(* ------------------------------------------------------ *)

(* Bomb explosion, means the refutors disavowed this tournament and do not trust the organizer. *)
(* In this case, the organizers funds return to the organizer, with the explosion tax left in the contract. *)
(* Insurers can then lazily grab the tez from this contract at their leisure. *)
let bomb_explosion (self, storage : address * storage) : return = begin
    
    let prize_mutez : nat = storage.total_prize / 1mutez in   

    (* Take explostion tax out of prize. *)
    let prize_amount_less_fee : nat = match is_nat((prize_mutez * (storage.accuracy - storage.explosion_tax))) with
        None -> (failwith "PRIZE_OVERFLOW" : nat)
      | Some n -> n in

    (* Return the prize to organizer. *)
    let return : nat = (prize_amount_less_fee / storage.accuracy) in

    let return_amount : tez = return * 1mutez in

    let return_tez : operation = flush(return_amount, storage.owner) in

    ([return_tez], { storage with stage = (Exploded : escrow_stage); })
end

(* Bomb defused, means the refutors did not reach a consensus on disavowal. Organizer's posted list is now confirmed. *)
(* In this case, the tez will be dispersed to the winners. With an insurance tax left over. *)
(* Insurers can then lazily grab their tez from this contract at their leisure. *)
let bomb_defused (self, storage : address * storage) : return = begin
  
    let prize_mutez : nat = storage.total_prize / 1mutez in   

    (* Take insurance tax out of prize. *)
    let prize_amount_less_fee : nat = match is_nat((prize_mutez * (storage.accuracy - storage.successful_defuse_tax))) with
        None -> (failwith "PRIZE_OVERFLOW" : nat)
      | Some n -> n / storage.accuracy in

    let payout_list : address list = match storage.results with
        None -> (failwith "NO_LIST" : address list)
      | Some p -> p in

    let payout_txs : operation list = pay_out(payout_list, storage.prize_structure, prize_amount_less_fee, storage.accuracy, self, ([] : operation list)) in

    (payout_txs, { storage with stage = (Exploded : escrow_stage); })
end


(* Technically, anyone can call this entrypoint, but usually it'll be the tourney organizer who holds the original key *)
(* If the organizer runs away for any reason, this can be called via someone solving the timelock puzzle. *)
let bomb (self, revealed, storage : address * bytes * storage) : return = begin

    assert_msg (is_posted(storage.stage) = true, "MUST_BE_POSTED");

    let now : timestamp = Tezos.get_now() in
    let user : address = Tezos.get_sender() in

    (* Ensure refuation phase has passed its time limit. *)
    assert_msg (now > storage.stage_timestamp + int(storage.refutation_sec), "REFUTATION_STILL_OPEN");

    let refutation_balance : nat = storage.refutation_amt in

    let refutation_pct : nat = storage.refutation_pct * storage.accuracy in

    let chest_found : bytes  = match storage.locked_bomb with
      None -> (failwith "NO_CHEST" : bytes)
    | Some p -> p in

    (* Final Action *)
    match validate_secret(chest_found, revealed) with
      | Some _ -> if refutation_balance >= refutation_pct then bomb_explosion(self, storage) else bomb_defused(self, storage)
      | None -> (failwith "BOMB_FATAL" : return)
end

(* Gets the users insurance token amount and exits the pool with some XTZ. *)
let exit (self, storage : address * storage) : return = begin

    (* This means the leftover funds in contract are for insurers, and should have some fees on top. *)
    assert_msg (is_exploded(storage.stage) = true, "MUST_BE_EXPLODED");

    let now : timestamp = Tezos.get_now() in
    let user : address = Tezos.get_sender() in

    let leftovers : tez = Tezos.get_balance() in
    let leftovers_mutez : nat = leftovers / 1mutez in

    (* Get full balance amount for adding to total. *)
    let tokens_from_user : nat = get_balance_immediate(user, storage.assets.ledger) in

    (* Get total supply *)
    let all_tokens : nat = get_supply_immediate(storage.assets.token_total_supply) in

    (* Find % of XTZ balance owned. *)
    let pct_of_all_tokens : nat = (tokens_from_user * storage.accuracy) / all_tokens in

    let amount_mutez : nat = (pct_of_all_tokens * leftovers_mutez) / storage.accuracy in

    let amount_tez : tez = amount_mutez * 1mutez in

    let pay_amount : operation = flush(amount_tez, user) in

    (* Burn the users tokens as they withdraw tez. *)
    let new_assets_burn = burn_tokens([{
      owner = user;
      token_id = 0n;
      amount = tokens_from_user;
    }], storage.assets) in

    ([pay_amount], { storage with assets = new_assets_burn; })
end

type escrow_entrypoints =
  | Init        of initialize_game
  | Insure      of tez
  | Resolve of address list
  | Refute      of unit
  | Bomb        of bytes
  | Exit        of unit

let smart_bomb_main (p , storage : escrow_entrypoints * storage) : return = begin
  let self : address = Tezos.get_self_address() in
  match p with
    | Init    i ->   init(self, i, storage)
    | Insure  i -> insure(self, i, storage)
    | Resolve r ->   post(self, r, storage)
    | Refute  u -> refute(self, storage)
    | Bomb    b ->   bomb(self, b, storage)
    | Exit    u ->   exit(self, storage)
end