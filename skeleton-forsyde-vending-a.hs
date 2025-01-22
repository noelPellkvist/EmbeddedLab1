module VendingMachine where

import ForSyDe.Shallow

vendingMachine :: Signal Bool -- Signal of 5 SEK coins
               -> Signal Bool -- Signal of 10 SEK coins
               -> Signal (Bool, Bool) -- Signal of (Bottle, Return)

-- Your code

s_coin5 =  signal [False, True, True, True, False, False]
s_coin10 = signal [True, False, False, False, True, False]
