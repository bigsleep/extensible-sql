{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module ExSql.Syntax.Internal
    ( FieldsSelector(..)
    , KnownConstructor(..)
    , PersistConvert
    , RRef(..)
    , Ref(..)
    , RelationAlias(..)
    , ResultType
    , Sel(..)
    , SelWithAlias(..)
    , ValueList
    , pattern Nil
    , pattern (:<)
    , relationAliasId
    , rrefId
    ) where

import Control.Monad.Trans.State.Strict (StateT)
import qualified Data.Extensible.HList as HList (HList(..))
import Data.Text (Text)
import Database.Persist (Entity, PersistEntity(..), PersistField(..),
                         PersistValue(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types

data RelationAlias a where
    RelationAlias :: (PersistEntity a) => Int -> RelationAlias (Entity a)
    RelationAliasSub :: Int -> FieldsSelector (Sel g) a -> RelationAlias a

relationAliasId :: RelationAlias a -> Int
relationAliasId (RelationAlias i)      = i
relationAliasId (RelationAliasSub i _) = i

data Sel g a where
    Star :: RelationAlias a -> Sel g a
    Sel :: (PersistField a) => g a -> Sel g a

data SelWithAlias g a where
    Star' :: RelationAlias a -> SelWithAlias g a
    Sel' :: (PersistField a) => g a -> FieldAlias a -> SelWithAlias g a

instance Hoist Sel where
    hoist _ (Star a) = Star a
    hoist f (Sel a)  = Sel (f a)

instance Hoist SelWithAlias where
    hoist _ (Star' a)  = Star' a
    hoist f (Sel' a b) = Sel' (f a) b

data FieldsSelector g x where
    Raw :: FieldsSelector g [PersistValue]
    Nullable :: FieldsSelector g a -> FieldsSelector g (Maybe a)
    (:$:) :: (KnownConstructor (ResultType b), ConstructorType (ResultType b) ~ (a -> b)) => (a -> b) -> g a -> FieldsSelector g b
    (:*:) :: (KnownConstructor (ResultType b)) => FieldsSelector g (a -> b) -> g a -> FieldsSelector g b

infixl 4 :$:, :*:

instance Hoist FieldsSelector where
    hoist _ Raw          = Raw
    hoist f (Nullable a) = Nullable (hoist f a)
    hoist f (g :$: a)    = g :$: f a
    hoist f (s :*: a)    = hoist f s :*: f a

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

instance KnownConstructor (a, b) where
    type ConstructorType (a, b) = (a -> b -> (a, b))

instance KnownConstructor (a, b, c) where
    type ConstructorType (a, b, c) = (a -> b -> c -> (a, b, c))

instance KnownConstructor (a, b, c, d) where
    type ConstructorType (a, b, c, d) = (a -> b -> c -> d -> (a, b, c, d))

instance KnownConstructor (Entity a) where
    type ConstructorType (Entity a) = (Entity a -> Entity a)
