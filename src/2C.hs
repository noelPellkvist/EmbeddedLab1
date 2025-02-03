module Main where

import ForSyDe.Shallow

system :: Signal Int -> Signal Int -> (Signal Int, Signal Int, Signal Int, Signal Int, Signal Int, Signal Int)
system s_in1 s_in2 = (s_1, s_2, s_3, s_4, s_4_delayed, s_out) where
  s_1 = actor_a s_in1
  s_2 = actor_b s_in2
  s_3 = actor_c s_1 s_4_delayed
  (s_4, s_out) = actor_d s_2 s_3
  s_4_delayed = delaySDF [0] s_4  

actor_a :: Signal Int -> Signal Int
actor_a = actor11SDF 2 1 f_1 where
  f_1 [x, y] = [x + y]

actor_b :: Signal Int -> Signal Int
actor_b = actor11SDF 1 2 f_2 where
  f_2 [x] = [x, x+1]

actor_c :: Signal Int -> Signal Int -> Signal Int
actor_c = actor21SDF (2,1) 1 f_3 where
  f_3 [x, y] [z] = [x + y + z]

actor_d :: Signal Int -> Signal Int -> (Signal Int, Signal Int)
actor_d = actor22SDF (2,1) (1,2) f_4 where
  f_4 [x, y] [z] = ([x + y + z], [x + y, x + y + z]) 


main :: IO ()
main = do
    putStrLn "Running the program"

    let i_1 = signal [0,1,2,3,4,5,6,7,8,9]
    let i_2 = signal [0,10,20,30,40,50,60,70,80,90]

    -- Simulate the system
    let (s_1, s_2, s_3, s_4, s_4_delayed, s_out) = system i_1 i_2


    putStrLn "S_1 signal:"
    putStrLn $ show s_1

    putStrLn "S_2 signal:"
    putStrLn $ show s_2

    putStrLn "S_3 signal:"
    putStrLn $ show s_3

    putStrLn "S_4 signal:"
    putStrLn $ show s_4

    putStrLn "S_4_Delayed signal:"
    putStrLn $ show s_4_delayed

    putStrLn "Output signal:"
    putStrLn $ show s_out