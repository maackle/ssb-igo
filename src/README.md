

## types

```
data IgoMsg
  = RequestMatch GameTerms
  | ExpireRequest MsgKey
  | OfferMatch OfferMatchPayload
  | WithdrawOffer MsgKey
  | AcceptMatch AcceptMatchPayload
  | DeclineMatch MsgKey
  | PlayMove PlayMovePayload
  | Kibitz KibitzPayload
```

### RequestMatch

A public notice, "hey, I'd like to play a game with the following terms!"

### ExpireRequest

Targets your own previous RequestMatch. "Never mind!"

### OfferMatch

A message to a single recipient offering a game with terms

Complex version:
Generate a new encryption key, break it in half. Message includes:
- First half of key as plaintext
- Second half of key encrypted with recipient's public key
- Rest of payload encrypted with that shared key

This way, anyone can verify that the game terms match, but only after
the second party has accepted

### WithdrawOffer

Cancels a previous offer

### AcceptMatch

A public response to the private OfferMatch, which declares "game on!"
This message is the root of any game

### DeclineMatch (PRIVATE)

Used to privately answer "no thanks" to a privately received OfferMatch.

### InvalidateGame

Only used if someone fraudulently creates an AcceptMatch
