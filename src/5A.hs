import ForSyDe.Shallow

selectSystem :: Signal Int -> Signal Int -> Signal Bool -> Signal Int
selectSystem s_t s_f s_bool = s_out
  where
    c_1  = d_select s_bool
    s_out = kernel21SADF c_1 s_t s_f

d_select = detector11SADF consume_rate next_state select_scenario initial_state
  where
    consume_rate = 1

    next_state _ [b] = if b then 0 else 1

    scenario_0 = ( (1, 1)
                 , 1
                 , \ts fs -> ts
                 )

    scenario_1 = ( (1, 1)
                 , 1
                 , \ts fs -> fs
                 )

    select_scenario 0 = (1, [scenario_0])
    select_scenario 1 = (1, [scenario_1])

    initial_state = 0

switchSystem :: Signal Int -> Signal Bool -> (Signal Int, Signal Int)
switchSystem s_in s_bool = (s_out_1, s_out_2)
  where
    c_1 = d_switch s_bool

    (s_out_1, s_out_2) = kernel12SADF c_1 s_in

d_switch = detector11SADF consume_rate next_state select_scenario initial_state
  where
    consume_rate = 1

    next_state _ [b] = if b then 0 else 1

    scenario_0 = (1, (1,0), \[x] -> ([x], []))
    scenario_1 = (1, (0,1), \[x] -> ([], [x]))

    select_scenario 0 = (1, [scenario_0])
    select_scenario 1 = (1, [scenario_1])

    initial_state = 0


main :: IO ()
main = do
  let s_t    = signal [1,2,3,4,5]
  let s_f    = signal [6,7,8,9,10]
  let s_bool = signal [True, True, False, False, True]

  let s_out_select  = selectSystem s_t s_f s_bool
  putStrLn "== Output from select =="
  print s_out_select

  let s_in    = signal [1,2,3,4,5]
  let s_bool = signal [True, True, False, False, True]

  let (s_out_switch_1, s_out_switch_2)  = switchSystem s_in s_bool
  putStrLn "== Output from switch (T) =="
  print s_out_switch_1
  putStrLn "== Output from switch (F) =="
  print s_out_switch_2
