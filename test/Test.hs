{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import SafeWildCards
import TestTypes (Rec (Rec1, Rec2), RecFamily (RecFamily1))

main :: IO ()
main = do
  fieldsSpec
  fieldsPrefixedSpec

----------------------------------------------------------------------------
-- fields
----------------------------------------------------------------------------

f :: Rec -> [Int]
f $(fields 'Rec1) = [a, b]
f $(fields 'Rec2) = [a, b, c]

fFamily :: RecFamily -> [Int]
fFamily $(fields 'RecFamily1) = [af, bf]

fieldsSpec :: IO ()
fieldsSpec = do
  f (Rec1 1 2) `mustBe` [1, 2]
  f (Rec2 1 2 3) `mustBe` [1, 2, 3]
  fFamily (RecFamily1 1 2) `mustBe` [1, 2]

----------------------------------------------------------------------------
-- fieldsPrefixed
----------------------------------------------------------------------------

p :: Rec -> Rec -> [Int]
p $(fieldsPrefixed "r1_" 'Rec1) $(fieldsPrefixed "r2_" 'Rec2) =
  [r1_a, r1_b, r2_a, r2_b, r2_c]
p _ _ = undefined

pFamily :: RecFamily -> RecFamily -> [Int]
pFamily $(fieldsPrefixed "r1_" 'RecFamily1) $(fieldsPrefixed "r2_" 'RecFamily1) =
  [r1_af, r1_bf, r2_af, r2_bf]

fieldsPrefixedSpec :: IO ()
fieldsPrefixedSpec = do
  p (Rec1 1 2) (Rec2 3 4 5) `mustBe` [1, 2, 3, 4, 5]
  pFamily (RecFamily1 1 2) (RecFamily1 3 4) `mustBe` [1, 2, 3, 4]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

mustBe :: (Eq a, Show a) => a -> a -> IO ()
mustBe x y
  | x == y = return ()
  | otherwise = error $ show x ++ " must be " ++ show y
