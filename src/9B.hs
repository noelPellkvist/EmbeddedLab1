import ForSyDe.Shallow

data Coin   = C5 | C10           deriving (Show, Eq, Ord)
data Bottle = B                  deriving (Show, Eq, Ord)
data Return = R                  deriving (Show, Eq, Ord)

type Coin_Event   = AbstExt Coin
type Bottle_Event = AbstExt Bottle
type Return_Event = AbstExt Return

vendingMachine :: Signal Coin_Event -> Signal (Bottle_Event, Return_Event)
vendingMachine s_coin =
  let sState     = delaySY 0 sNextState
      sNextState = zipWithSY nextState sState s_coin
      sOut       = zipWithSY outFun sState s_coin
  in  sOut

nextState :: Int -> Coin_Event -> Int
nextState oldBal Abst      = oldBal
nextState oldBal (Prst c)  =
  let val   = case c of { C5 -> 5; C10 -> 10 }
      total = oldBal + val
  in  if total >= 10
        then 0    
        else total

outFun :: Int -> Coin_Event -> (Bottle_Event, Return_Event)
outFun oldBal Abst = (Abst, Abst)
outFun oldBal (Prst c) =
  let val   = case c of { C5 -> 5; C10 -> 10 }
      total = oldBal + val
  in if total < 10
       then (Abst, Abst)       
     else if total == 10
       then (Prst B, Abst)      
       else (Prst B, Prst R)    

main :: IO ()
main = do
  putStrLn "Starting up"

  let s_coin = signal
        [ Prst C10      
        , Prst C5       
        , Prst C5      
        , Prst C5       
        , Prst C10      
        , Abst          
        ]
      s_vm = vendingMachine s_coin

  putStrLn "All outputs (Bottle_Event, Return_Event):"
  putStrLn (show s_vm)

  let (s_bottle, s_return) = unzipSY s_vm
  putStrLn "Bottles timeline:"
  putStrLn (show s_bottle)
  putStrLn "Returns timeline:"
  putStrLn (show s_return)
