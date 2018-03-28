-- 逆波兰表达式计算器
-- example: eval "1 2 +"
--          eval "1 2 3 * +"
--          eval "1 2 3 4 5 - + / *"

module Rpn(
eval
)where

import qualified Data.Map as Map
import qualified Data.List as List

eval :: String -> Double
eval = eval' [] . words

eval' :: [Double] -> [String] -> Double
eval' = \s expr -> (\(x:[]) -> x) $ List.foldl' (\ stack e -> case Map.lookup e op of
                                              Just (f, cnt) -> let (h, t) = splitAt cnt stack in (f h):t
                                              Nothing -> let v = read e :: Double in v:stack
                                            ) s expr

op :: Map.Map String ([Double]->Double, Int)
op = Map.fromList [
  ("+", (\(a:b:[]) -> b+a, 2))
  , ("-", (\(a:b:[]) -> b-a, 2))
  , ("*", (\(a:b:[]) -> b*a, 2))
  , ("/", (\(a:b:[]) -> b/a, 2))]
