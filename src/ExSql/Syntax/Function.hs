{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
module ExSql.Syntax.Function
    ( Function(..)
    ) where

import ExSql.Syntax.Class

import Data.Text (Text)

data Function g x where
    Function0 :: Text -> Function g x
    Function1 :: Text -> g a0 -> Function g x
    Function2 :: Text -> g a0 -> g a1 -> Function g x
    Function3 :: Text -> g a0 -> g a1 -> g a2 -> Function g x
    Function4 :: Text -> g a0 -> g a1 -> g a2 -> g a3 -> Function g x
    Function5 :: Text -> g a0 -> g a1 -> g a2 -> g a3 -> g a4 -> Function g x
    Function6 :: Text -> g a0 -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> Function g x

instance Hoist Function where
    hoist _ (Function0 n) = Function0 n
    hoist f (Function1 n a) = Function1 n (f a)
    hoist f (Function2 n a0 a1) = Function2 n (f a0) (f a1)
    hoist f (Function3 n a0 a1 a2) = Function3 n (f a0) (f a1) (f a2)
    hoist f (Function4 n a0 a1 a2 a3) = Function4 n (f a0) (f a1) (f a2) (f a3)
    hoist f (Function5 n a0 a1 a2 a3 a4) = Function5 n (f a0) (f a1) (f a2) (f a3) (f a4)
    hoist f (Function6 n a0 a1 a2 a3 a4 a5) = Function6 n (f a0) (f a1) (f a2) (f a3) (f a4) (f a5)
