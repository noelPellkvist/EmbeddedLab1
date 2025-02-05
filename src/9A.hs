import ForSyDe.Shallow

delay5 :: Signal Bool -> Signal Bool
delay5 = delaySY False

hasFive :: Signal Bool -> Signal Bool -> Signal(Bool, Bool)
hasFive current5 last5 =
  mapSY (\(b1, b2) ->
    if b1 && b2
      then (False, True)             -- case: both are True
      else if b1 || b2
        then (True, False)           -- case: only b1 is True
        else (False, False)
  ) (zipSY current5 last5)

hasTen :: Signal Bool -> Signal Bool -> Signal Bool
hasTen tenFromFives coin10 =
  mapSY (\(b1, b2) -> if b1 || b2 then True else False)
        (zipSY tenFromFives coin10)


-- input here is coin_5 and output should be coin5 and coin_5 delayed
coin5Propogator :: Signal Bool -> (Signal Bool, Signal Bool)
coin5Propogator coin5 = (x, y)
  where
    delayedcoin5 = delay5 has5
    x = has5
    y = c5_to_10
    (has5, c5_to_10) = unzipSY $ hasFive coin5 delayedcoin5


calculateOutput :: Signal Bool -> Signal Bool -> Signal(Bool, Bool)
calculateOutput coin5 coin10 =
  mapSY (\(b1, b2) ->
    if b1 && b2 --we have both coins, output bottle and change
      then (True, True)             
      else if b1 && not b2 --we have a 5 but no 10, dont output anything
        then (False, False)   
        else if not b1 && b2 -- we have a 10 coin but no 5 coin, output bottle without change
          then (True, False)
          else (False, False) --we dont have any coins, dont give anything
  ) (zipSY coin5 coin10)


vendingMachine :: Signal Bool             -- ^ 5 SEK coin
               -> Signal Bool             -- ^ 10 SEK coin
               -> Signal (Bool, Bool)     -- ^ (bottle, return)
vendingMachine c5 c10 = output where
  (stored5, acc10) = coin5Propogator c5
  coin10 = hasTen acc10 c10
  output = calculateOutput stored5 coin10

main :: IO ()
main = do
  let coin5 = signal [False, True, True, True, False, False]
  let coin10 = signal [True, False, False, False, True, False]
  let (bottle, change) = unzipSY $ vendingMachine coin5 coin10
  putStrLn $ show bottle
  putStrLn $ show change
