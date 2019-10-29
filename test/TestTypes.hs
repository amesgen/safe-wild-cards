{-# LANGUAGE TypeFamilies #-}

module TestTypes (Rec(..), RecFamily(..)) where

data Rec
  = Rec1 { a :: Int, b :: Int }
  | Rec2 { a :: Int, b :: Int, c :: Int }

data family RecFamily

data instance RecFamily = RecFamily1 { af :: Int, bf :: Int }
