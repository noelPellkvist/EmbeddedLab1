import ForSyDe.Shallow

evenActor :: Signal Int -> Signal Int
evenActor = actor11SDF 2 1 (\[x, _] -> [x])

oddActor :: Signal Int -> Signal Int
oddActor = actor11SDF 2 1 (\[_, y] -> [-y])

system :: Signal Int -> (Signal Int, Signal Int)
system s_in = (s_out1, s_out2) where
  s_out1 = evenActor s_in
  s_out2 = oddActor s_in

main :: IO ()
main = do
  let s_test = signal [0,1,2,3,4,5,6,7,8,9,10]
  let (s_out1, s_out2) = system s_test
  putStrLn "Even-indexed output:"
  putStrLn $ show s_out1
  putStrLn "Odd-indexed output:"
  putStrLn $ show s_out2
