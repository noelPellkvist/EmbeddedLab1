import ForSyDe.Shallow


p1 :: Signal Int -> Signal Int -> Signal Int
p1 = zipWithSY (+)

p2 :: Signal Int -> Signal Int
p2 = mapSY (*2)

p3 :: Signal Int -> Signal Int -> Signal Int
p3 = zipWithSY (+)

p4 :: Signal Int -> Signal Int
p4 = delaySY 0

p5 :: Signal Int -> Signal Int
p5 = mapSY (+1)

topLevel :: Signal Int -> (Signal Int, Signal Int)
topLevel input = (x, y)
  where
    y = p5 input
    x = p4 ( p3 ( p2 ( p1 input x ) )
                 ( p5 input )
            )

main :: IO ()
main = do
  let input = signal [1,2,3]
  let (x, y) = topLevel input
  putStrLn $ show input
  putStrLn $ show x
  putStrLn $ show y
