-- 逆波兰表达式计算器
-- example: eval "1 2 +"
--          eval "1 2 3 * +"
--          eval "1 2 3 4 5 - + / *"
-- 你可以扩展他，例如加入++：
-- op = Map.fromList [... ("++", (\(a:[]) -> a + 1, 1))]
-- 加入sqrt: op = Map.fromList [... ("sqrt", (\(a:[]) -> sqrt a , 1))]
-- 如果你希望扩展那些多于1元数的运算符，一定要记住，\args->..的args列表是逆序的，所以有：\(a:b:[]) -> b/a
-- 例如加入mod: 
-- op = Map.fromList [... ("mod", (\(a:b:[]) -> mod b a, 2))]
-- 不幸的是，上面的例子是错的，你需要先将a b转为Int, 再将结果转Double

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
