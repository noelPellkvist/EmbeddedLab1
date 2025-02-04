import ForSyDe.Shallow
import Debug.Trace  -- Import debug utilities

system s_in=(s_out_1,s_out_2) where
  (s_out_1,s_out_2)= k_1 c_1 s_in
  c_1 = d_1 s_in

k_1=kernel12SADF

-- Detector 'd_1' with Debugging
d_1 = detector11SADF consume_rate next_state
                    select_scenario initial_state where
  consume_rate =1
  --NextStateFunction'next_state' ignoresinputvalue
  next_state 0 _ =1
  next_state 1 _ =0
  --Definitionofscenarios
  ---Scenario0:Sendtoken
  k_1_scenario_0= (1,(1,0),\[x]-> ([x],[]))
  ---Scenario1:Ignoretoken
  k_1_scenario_1= (1,(0,1),\[x]-> ([],[-x]))
  --FunctionforSelectionofscenarios
  select_scenario 0 =(1,[k_1_scenario_0])
  select_scenario 1 =(1,[k_1_scenario_1])
  --InitialState
  initial_state =0

-- Main Function to Test Detector
main :: IO ()
main = do
  let (s_1, s_2) = system $ takeS 10 $ infiniteS (+1) 0
  putStrLn $ show s_1
  putStrLn $ show s_2
