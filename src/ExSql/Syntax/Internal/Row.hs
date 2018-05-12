{-# LANGUAGE GADTs, KindSignatures #-}
module ExSql.Syntax.Internal.Row
    ( Row(..)
    , row
    , row2
    , row3
    , row4
    , row5
    , row6
    ) where

import Database.Persist (PersistField(..))
import ExSql.Syntax.Class

-- row construcor
data Row (g :: * -> *) a where
    Row :: (PersistField a) => g a -> Row g a
    Row2 :: (PersistField a0, PersistField a1) => (g a0, g a1) -> Row g (a0, a1)
    Row3 :: (PersistField a0, PersistField a1, PersistField a2) => (g a0, g a1, g a2) -> Row g (a0, a1, a2)
    Row4 :: (PersistField a0, PersistField a1, PersistField a2, PersistField a3) => (g a0, g a1, g a2, g a3) -> Row g (a0, a1, a2, a3)
    Row5 :: (PersistField a0, PersistField a1, PersistField a2, PersistField a3, PersistField a4) => (g a0, g a1, g a2, g a3, g a4) -> Row g (a0, a1, a2, a3, a4)
    Row6 :: (PersistField a0, PersistField a1, PersistField a2, PersistField a3, PersistField a4, PersistField a5) => (g a0, g a1, g a2, g a3, g a4, g a5) -> Row g (a0, a1, a2, a3, a4, a6)

instance Hoist Row where
    hoist f (Row a) = Row (f a)
    hoist f (Row2 (a0, a1)) = Row2 (f a0, f a1)
    hoist f (Row3 (a0, a1, a2)) = Row3 (f a0, f a1, f a2)
    hoist f (Row4 (a0, a1, a2, a3)) = Row4 (f a0, f a1, f a2, f a3)
    hoist f (Row5 (a0, a1, a2, a3, a4)) = Row5 (f a0, f a1, f a2, f a3, f a4)
    hoist f (Row6 (a0, a1, a2, a3, a4, a5)) = Row6 (f a0, f a1, f a2, f a3, f a4, f a5)

row :: (PersistField a) => g a -> Row g a
row = Row

row2 :: (PersistField a0, PersistField a1) => (g a0, g a1) -> Row g (a0, a1)
row2 = Row2

row3 :: (PersistField a0, PersistField a1, PersistField a2) => (g a0, g a1, g a2) -> Row g (a0, a1, a2)
row3 = Row3

row4 :: (PersistField a0, PersistField a1, PersistField a2, PersistField a3) => (g a0, g a1, g a2, g a3) -> Row g (a0, a1, a2, a3)
row4 = Row4

row5 :: (PersistField a0, PersistField a1, PersistField a2, PersistField a3, PersistField a4) => (g a0, g a1, g a2, g a3, g a4) -> Row g (a0, a1, a2, a3, a4)
row5 = Row5

row6 :: (PersistField a0, PersistField a1, PersistField a2, PersistField a3, PersistField a4, PersistField a5) => (g a0, g a1, g a2, g a3, g a4, g a5) -> Row g (a0, a1, a2, a3, a4, a6)
row6 = Row6
