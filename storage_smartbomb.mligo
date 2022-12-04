#include "smartbomb.mligo"

let generate_storage : storage =

  let accuracy : nat = 1_000_000n in

  let one_tez : tez = 1mutez * accuracy in

  let empty_assets : multi_ft_token_storage = {      
    ledger = (Big_map.empty : ledger);      
    operators = (Big_map.empty : operator_storage);      
    token_total_supply = (Big_map.empty : token_total_supply);      
    token_metadata = (Big_map.empty : token_metadata_storage);    
  } in

  {
    assets = empty_assets;

    accuracy = accuracy;

    stage = (Pregame : escrow_stage);
    stage_timestamp = Tezos.get_now();

    owner = ("tz1WqzwqqGBtJucJFUxzaYMSUyPVK3vMDRUW": address);

    locked_bomb = (None : bytes option);

    successful_defuse_tax = 10_000n;
    explosion_tax = 50_000n;

    total_prize = 0mutez;
    prize_structure = ([] : nat list);

    results = (None : address list option);
    refutation_sec = 120n;
    refutation_amt = 0n;
    refutation_pct = 70n;
    refutors = (Set.empty : address set);

    insurance_pool_size = 0mutez;
    insurance_pool_limit = 1_000n * one_tez;
    insurance_pool_sec = 120n;
    max_insurance_stake_per_user = 1_000n * one_tez;
  }