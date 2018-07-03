{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module ExSql.Syntax.Internal.Types
    ( FRef(..)
    , FieldAlias(..)
    , FieldsSelector(..)
    , KnownConstructor(..)
    , PersistConvert
    , RRef(..)
    , Ref(..)
    , RelationAlias(..)
    , ResultType
    , SResult
    , Sel(..)
    , SelWithAlias(..)
    , ValueList
    , pattern Nil
    , pattern (:<)
    ) where

import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Extensible.HList as HList (HList(..))
import Data.Text (Text)
import Database.Persist (Entity, PersistEntity(..), PersistField(..),
                         PersistValue(..))
import ExSql.Syntax.Class

data RelationAlias a where
    RelationAlias :: (PersistEntity a) => Int -> RelationAlias (Entity a)
    RelationAliasSub :: Int -> FieldsSelector Ref a -> RelationAlias a

data RRef a where
    RRef :: (PersistEntity a) => Int -> RRef (Entity a)
    RRefSub :: Int -> FieldsSelector Ref a -> RRef a

newtype FieldAlias a = FieldAlias Int
    deriving (Show, Eq)

data FRef a = FRef Int | QRef Int Int
    deriving (Show, Eq)

data Ref a where
    RelationRef :: RRef a -> Ref a
    FieldRef :: (PersistField a) => FRef a -> Ref a

data Sel g a where
    Star :: RelationAlias a -> Sel g a
    Sel :: (PersistField a) => g a -> Sel g a

data SelWithAlias g a where
    Star' :: RelationAlias a -> SelWithAlias g a
    Sel' :: (PersistField a) => g a -> FieldAlias a -> SelWithAlias g a

data ValueList a

type PersistConvert a = StateT [PersistValue] (Either Text) a

instance Hoist Sel where
    hoist _ (Star a) = Star a
    hoist f (Sel a)  = Sel (f a)

instance Hoist SelWithAlias where
    hoist _ (Star' a)  = Star' a
    hoist f (Sel' a b) = Sel' (f a) b

data FieldsSelector g x where
    Raw :: FieldsSelector g [PersistValue]
    (:$:) :: (KnownConstructor (SResult (ResultType b)), ConstructorType (SResult (ResultType b)) ~ (a -> b)) => (a -> b) -> g a -> FieldsSelector g b
    (:*:) :: (KnownConstructor (SResult (ResultType b))) => FieldsSelector g (a -> b) -> g a -> FieldsSelector g b

infixl 4 :$:, :*:

instance Hoist FieldsSelector where
    hoist _ Raw       = Raw
    hoist f (g :$: a) = g :$: f a
    hoist f (s :*: a) = hoist f s :*: f a

pattern Nil :: HList.HList h '[]
pattern Nil = HList.HNil

pattern (:<) :: h x -> HList.HList h xs -> HList.HList h (x ': xs)
pattern h :< xs = HList.HCons h xs

type family ResultType a where
    ResultType (a -> b) = ResultType b
    ResultType a = a

type family FunctionType (args :: [*]) result where
    FunctionType (x ': xs) r = x -> FunctionType xs r
    FunctionType '[] r = r

class KnownConstructor a where
    type ConstructorType a

data SResult a

instance KnownConstructor (SResult (a, b)) where
    type ConstructorType (SResult (a, b)) = (a -> b -> (a, b))

instance KnownConstructor (SResult (a, b, c)) where
    type ConstructorType (SResult (a, b, c)) = (a -> b -> c -> (a, b, c))

instance KnownConstructor (SResult (a, b, c, d)) where
    type ConstructorType (SResult (a, b, c, d)) = (a -> b -> c -> d -> (a, b, c, d))

instance KnownConstructor (SResult (Entity a)) where
    type ConstructorType (SResult (Entity a)) = (Entity a -> Entity a)
