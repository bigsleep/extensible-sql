{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module ExSql.Syntax.Join
    (
    ) where

import qualified Data.Extensible.HList as HList (HList(..))
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Type.Bool
import Data.Type.Equality
import ExSql.Syntax.Internal
import ExSql.Syntax.Internal.Types
import ExSql.Syntax.SelectQuery

newtype JoinProxy (j :: JoinType) t g a =
    JoinProxy (FromProxy t g a)

data Joins t g (xs :: [*]) where
    JNil :: Joins t g '[]
    JCons :: JoinProxy j t g a -> Joins t g xs -> Joins t g (JoinProxy j t g a ': xs)

data FromJoins t g a (xs :: [*]) where
    FromJoins :: FromProxy t g a -> Joins t g xs -> FromJoins t g a xs

type family HandleNullableType (nullable :: Bool) x :: * where
    HandleNullableType 'True (Maybe x) = Maybe x
    HandleNullableType 'True x = Maybe x
    HandleNullableType 'False x = x

type family IsMaybe (a :: *) :: Bool where
    IsMaybe (Maybe a) = 'True
    IsMaybe a = 'False

type family IsNotMaybe (a :: *) :: Bool where
    IsNotMaybe a = Not (IsMaybe a)

type family InList (x :: k) (xs :: [k]) :: Bool where
    InList x (x ': _) = 'True
    InList x (y ': xs) = InList x xs
    InList x '[] = 'False

class RightNullable (xs :: [*]) where
    type RightNullableType xs :: Bool
    rightNullable :: Joins t g xs -> Proxy (RightNullableType xs)

instance RightNullable '[] where
    type RightNullableType '[] = 'False
    rightNullable _ = Proxy

instance (RightNullable xs) => RightNullable (JoinProxy x t g a ': xs) where
    type RightNullableType (JoinProxy x t g a ': xs) = InList x ['RightJoin, 'FullJoin] || RightNullableType xs
    rightNullable _ = Proxy

class Selectable t => HandleFromProxy (t :: (* -> *) -> k -> *) g (n :: Bool) (a :: k) where
    type HandleFromProxyType t g n a :: *
    handleFromProxy :: Proxy n -> FromProxy t g a -> (From t g a -> SelectClause g) -> SelectQueryM g (HandleFromProxyType t g n a)

instance Selectable t => HandleFromProxy t g 'True a where
    type HandleFromProxyType t g 'True a = (t (Sel g) a, SelectRefType t a, RelationAlias (Maybe (SelectResultType t a)))
    handleFromProxy _ proxy sc = do
        i <- prepareFrom $ return ()
        let from_ = convertFromProxy i proxy
            (sel, sref, alias) = mkSelRefAlias from_
            alias' = nullableAlias alias
        tellSelectClause . sc $ from_
        return (sel, sref, alias')

        where
        nullableAlias = undefined

instance Selectable t => HandleFromProxy t g 'False a where
    type HandleFromProxyType t g 'False a = (t (Sel g) a, SelectRefType t a, RelationAlias (SelectResultType t a))
    handleFromProxy _ proxy sc = do
        i <- prepareFrom $ return ()
        let from_ = convertFromProxy i proxy
        tellSelectClause . sc $ from_
        return . mkSelRefAlias $ from_

class HandleJoins (n :: Bool) (xs :: [*]) where
    type NeedConvert n xs :: Bool
    type HandleJoinsType n xs :: [*]
    type LeftNullableType n xs :: Bool
    needConvert :: Proxy n -> Joins t g xs -> Proxy (NeedConvert n xs)
    leftNullable :: Proxy n -> Joins t g xs -> Proxy (LeftNullableType n xs)
    handleJoins :: Proxy n -> Joins t g xs -> SelectQueryM g (HList.HList Identity (HandleJoinsType n xs))

instance HandleJoins n '[] where
    type NeedConvert n '[] = 'False
    type HandleJoinsType n '[] = '[]
    type LeftNullableType n '[] = n
    needConvert _ _ = Proxy
    handleJoins _ _ = return HList.HNil

instance
    ( HandleJoins (n || InList x ['LeftJoin, 'FullJoin]) xs
    , RightNullable xs
    , HandleFromProxy t g ((n || x == 'FullJoin || RightNullableType xs) && IsNotMaybe a) a
    ) => HandleJoins n (JoinProxy x t g a ': xs) where
    type NeedConvert n (JoinProxy x t g a ': xs) = (n || x == 'FullJoin || RightNullableType xs) && IsNotMaybe a
    type HandleJoinsType n (JoinProxy x t g a ': xs) = HandleFromProxyType t g ((n || x == 'FullJoin || RightNullableType xs) && IsNotMaybe a) a ': HandleJoinsType (n || InList x ['LeftJoin, 'FullJoin]) xs
    type LeftNullableType n (JoinProxy x t g a ': xs) = n || InList x ['LeftJoin, 'FullJoin]
    needConvert _ _ = Proxy
    handleJoins n js @ (JCons (JoinProxy p) xs) = do
        let n' = needConvert n js
        a <- handleFromProxy n' p Join
        xs <- handleJoins (leftNullable n js) xs
        return (HList.HCons (return a) xs)

handleFromJoins :: (RightNullable xs, HandleFromProxy t g (RightNullableType xs) a, HandleJoins 'False xs) => FromJoins t g a xs -> SelectQueryM g (HList.HList Identity (HandleFromProxyType t g ('False || RightNullableType xs) a ': HandleJoinsType 'False xs))
handleFromJoins (FromJoins p js) = do
    a <- handleFromProxy (rightNullable js) p From
    xs <- handleJoins (Proxy :: Proxy 'False) js
    return (HList.HCons (return a) xs)
