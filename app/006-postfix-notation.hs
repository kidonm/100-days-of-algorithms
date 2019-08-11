{-# LANGUAGE LambdaCase #-}

data Expr
  = Lit Int
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving Show

fromLit = \case
  Lit l -> l

isOp = \case
  Lit _ -> False
  otherwise -> True

op :: Expr -> Expr -> Expr -> Expr
op op (Lit a) (Lit b) = Lit $ op' a b
  where
    op' = case op of
      Add -> (+)
      Sub -> (-)
      Mul -> (*)
      Div -> div
      Pow -> (^)

calc :: [Expr] -> Expr
calc = calc' []
  where
    calc' [result] [] = result
    calc' stack (e:es)
      | isOp e =
        let (s':s:ss) = stack
        in  calc' (op e s' s:ss) es
      | otherwise = calc' (e:stack) es

main = putStrLn $ show $ fromLit $ calc input
  where
    input = [Lit 1, Lit 2, Lit 3, Lit 4, Lit 5, Lit 6, Mul, Add, Sub, Div, Pow]
