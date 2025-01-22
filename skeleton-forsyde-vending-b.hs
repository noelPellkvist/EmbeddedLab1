module VendingMachine where

import ForSyDe.Shallow

data Coin = C5 | C10 deriving (Show, Eq, Ord)
data Bottle = B deriving (Show, Eq, Ord)
data Return = R deriving (Show, Eq, Ord)

type Coin_Event = AbstExt Coin
type Bottle_Event = AbstExt Bottle
type Return_Event = AbstExt Return

vendingMachine :: Signal Coin_Event -- Signal of Coins
               -> Signal (Bottle_Event, Return_Event) -- Signal of (Bottle, Return)

-- Your code

s_coin = signal [Prst C10, Prst C5, Prst C5, Prst C5, Prst C10, Abst]

