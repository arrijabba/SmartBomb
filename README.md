# SmartBomb (not SmartRank)

Usage

ligo compile contract smartbomb.mligo -e smart_bomb_main > ./smartbomb.tz

ligo compile storage storage_smartbomb.mligo -e smart_bomb_main 'generate_storage'

tezos-client originate contract smartbomb transferring 0 from %YOUR_ADDRESS% running ./smartbomb.tz --init '%STORAGE%' --burn-cap 3

--

