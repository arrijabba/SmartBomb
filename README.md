# SmartBomb (not SmartRank)

**SmartBomb** (not SmartRank) is an innovating escrow with a twist. SmartBomb uses a Michelson timelock chest that acts as a "bomb" which is set by the organizer.

After the organizer locks the tournament prize in the contract, external users can come in and provide payout insurance funds to the game.

*There is a major incentive for players to provide insurance. From a game theory perspective, this is what we want.*

Once the game is played (off-chain), users who hold **Insurance Pool** (IP) tokens can accuse the organizer of posting a false list.

If the accusations get to a certain point, the prize is called off and the organizer takes a 5% haircut on the prize (the rest goes back to organizer).

This result is a **Bomb Explosion**, defuse is blocked by user accusations. Users recieve the Explosion tax spread across all pool members.

The more likely result is a **Bomb Defusal** by the organizer. Insurance providers will take no action if they think everything is ok, and they will be provided with a 1% insurance tax off the top of the prize pool for participating in insurance.

For roadmap, unfortunately timelock is currently **rekt** on Tezos so I had to use a lame commit-reveal. 

The Timelock addition would be cool because it would protect from a tournement organizer running away or hiding in the Bahamas because anyone can eventually solve the lock and explode/defuse the bomb.

Parameters and fees obviously subject to tinkering. Contract is heavily commented so look there for technicals.

Check the working happy path at [KT1BdNUdak4Wg2hc7sxzKnigCdBMTkpu6Evy Ghostnet](https://better-call.dev/ghostnet/KT1BdNUdak4Wg2hc7sxzKnigCdBMTkpu6Evy/operations).

--

Usage

```
ligo compile contract smartbomb.mligo -e smart_bomb_main > ./smartbomb.tz

ligo compile storage storage_smartbomb.mligo -e smart_bomb_main 'generate_storage'

tezos-client originate contract smartbomb transferring 0 from %YOUR_ADDRESS% running ./smartbomb.tz --init '%STORAGE%' --burn-cap 3
```
--

