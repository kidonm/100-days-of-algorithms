type Input = (Int, Int)
newtype State = State (Input -> (State, Int))

c0 :: State
c0 = State c0'
  where
    c0' (1, 1) = (c1, 0)
    c0' (0, 0) = (c0 , 0)
    c0' _ = (c0 , 1)

c1 :: State
c1 = State c1'
  where
    c1' (1, 1) = (c1, 1)
    c1' (0, 0) = (c0 , 1)
    c1' _ = (c1 , 0)

run :: [(Int, Int)] -> State -> [Int]
run [] _ = []
run (i:is) (State f) =
  let (f', o) = f i
  in o:run is f'
