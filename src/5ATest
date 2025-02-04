import ForSyDe.Shallow

switch :: Signal Int -> Signal Bool -> (Signal Int, Signal Int)
switch s_in s_bool = (s_out_1, s_out_2)
  where
    c_1 = d_switch s_bool

    (s_out_1, s_out_2) = k_1 c_1 s_in

d_switch = detector11SADF consume_rate next_state select_scenario initial_state
  where
    consume_rate = 1

    next_state _ [b] = if b then 0 else 1

    scenario_0 = (1, (1,0), \[x] -> ([x], []))
    scenario_1 = (1, (0,1), \[x] -> ([], [x]))

    select_scenario 0 = (1, [scenario_0])
    select_scenario 1 = (1, [scenario_1])

    initial_state = 0

k_1 = kernel12SADF

main :: IO ()
main = do 
  let s_in   = takeS 10 $ infiniteS (+1) 0
  let s_bool = signal [True, True, True, True, True, False, True, True, True, True]
  let (s_out_1, s_out_2) = switch s_in s_bool
  putStrLn "=== s_out_1 ==="
  print s_out_1
  putStrLn "=== s_out_2 ==="
  print s_out_2
