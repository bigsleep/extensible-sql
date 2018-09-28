{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module ExSql.Syntax.Internal
    ( FieldClause(..)
    , FieldsSelector(..)
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

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as State (evalState,
                                                            execState, get,
                                                            mapStateT, modify',
                                                            put, runState)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Extensible.HList as HList (HList(..))
import Data.List (uncons)
import Data.Proxy (Proxy(..))
import qualified Database.Persist as Persist (Entity(..), PersistEntity(..),
                                              PersistField(..),
                                              PersistValue(..))
import qualified Database.Persist.Sql.Util as Persist (entityColumnCount,
                                                       parseEntityValues)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types
import Safe.Exact (splitAtExactMay)

class Hoist t => Selectable (t :: (* -> *) -> k -> *) where
    type SelectResultType t (a :: k) :: *
    type SelectRefType t (a :: k) :: *
    mkPersistConvert :: t (Sel g) a -> PersistConvert (SelectResultType t a)
    mkFieldClausesAndRef :: t (Sel g) a -> (SelectRefType t a, [FieldClause g])
    countFields :: Int -> t (Sel g) a -> Int

data RelationAlias a where
    RelationAlias :: (Persist.PersistEntity a) => Int -> RelationAlias (Persist.Entity a)
    RelationAliasSub :: Int -> FieldsSelector (Sel g) a -> RelationAlias a

relationAliasId :: RelationAlias a -> Int
relationAliasId (RelationAlias i)      = i
relationAliasId (RelationAliasSub i _) = i

data Sel g a where
    Star :: RelationAlias a -> Sel g a
    Sel :: (Persist.PersistField a) => g a -> Sel g a

data SelWithAlias g a where
    Star' :: RelationAlias a -> SelWithAlias g a
    Sel' :: (Persist.PersistField a) => g a -> FieldAlias a -> SelWithAlias g a

data FieldClause g where
    FieldClause :: SelWithAlias g a -> FieldClause g

instance Hoist Sel where
    hoist _ (Star a) = Star a
    hoist f (Sel a)  = Sel (f a)

instance Hoist SelWithAlias where
    hoist _ (Star' a)  = Star' a
    hoist f (Sel' a b) = Sel' (f a) b

data FieldsSelector g x where
    Raw :: FieldsSelector g [Persist.PersistValue]
    Nullable :: FieldsSelector g a -> FieldsSelector g (Maybe a)
    (:$:) :: (KnownConstructor (ResultType b), ConstructorType (ResultType b) ~ (a -> b)) => (a -> b) -> g a -> FieldsSelector g b
    (:*:) :: (KnownConstructor (ResultType b)) => FieldsSelector g (a -> b) -> g a -> FieldsSelector g b

infixl 4 :$:, :*:

instance Hoist FieldsSelector where
    hoist _ Raw          = Raw
    hoist f (Nullable a) = Nullable (hoist f a)
    hoist f (g :$: a)    = g :$: f a
    hoist f (s :*: a)    = hoist f s :*: f a

instance HTraversable FieldsSelector where
    htraverse _ Raw          = pure Raw
    htraverse f (Nullable a) = Nullable <$> htraverse f a
    htraverse f (g :$: a)    = (g :$:) <$> f a
    htraverse f (s :*: a)    = (:*:) <$> htraverse f s <*> f a

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

instance KnownConstructor (Persist.Entity a) where
    type ConstructorType (Persist.Entity a) = (Persist.Entity a -> Persist.Entity a)

instance Selectable FieldsSelector where
    type SelectResultType FieldsSelector a = a
    type SelectRefType FieldsSelector a = FieldsSelector Ref a

    mkPersistConvert Raw =  do
        xs <- State.get
        State.put mempty
        return xs
    mkPersistConvert (Nullable a) = do
        xs <- State.get
        let l = length xs
            c = countFields l a
        State.mapStateT (handleNullable xs c) (mkPersistConvert a)
        where
        handleNullable _ _ (Right (x, s)) = Right (Just x, s)
        handleNullable vs c (Left HitNullValue) = do
            (_, rest) <- maybe (Left . ConvertError $ "not enough input values") return (splitAtExactMay c vs)
            return (Nothing, rest)
        handleNullable _ _ (Left (ConvertError e)) = Left (ConvertError e)
    mkPersistConvert (f :$: Star a @ RelationAlias {}) = f <$> mkPersistConvertEntity a
    mkPersistConvert (f :$: Star (RelationAliasSub _ s)) = f <$> mkPersistConvert s
    mkPersistConvert (f :$: Sel {}) = f <$> mkPersistConvertField Proxy
    mkPersistConvert (s :*: Star a @ RelationAlias {}) = mkPersistConvert s <*> mkPersistConvertEntity a
    mkPersistConvert (s0 :*: Star (RelationAliasSub _ s1)) = mkPersistConvert s0 <*> mkPersistConvert s1
    mkPersistConvert (s :*: Sel {}) = do
        f <- mkPersistConvert s
        f <$> mkPersistConvertField Proxy

    mkFieldClausesAndRef x =
        let (ref, xs) = flip State.runState mempty . htraverse putFieldClause . flip State.evalState 0 . htraverse mkSelWithAlias $ x
        in (ref, DList.toList xs)
        where
        putFieldClause a = do
            let fc = FieldClause a
            State.modify' (`DList.snoc` fc)
            return . aliasToRef $ a

        aliasToRef :: SelWithAlias g a -> Ref a
        aliasToRef (Star' a)               = RelationRef (RRef (relationAliasId a))
        aliasToRef (Sel' _ (FieldAlias i)) = FieldRef (FRef i)

    countFields n =
        flip State.execState 0 . htraverse (countField n)

mkSelWithAlias :: Sel g a -> State Int (SelWithAlias g a)
mkSelWithAlias (Star a) = return (Star' a)
mkSelWithAlias (Sel a) = do
    i <- State.get
    State.put (i + 1)
    return (Sel' a (FieldAlias (i + 1)))

countField :: Int -> Sel g a -> State Int (Sel g a)
countField _ x @ (Star a @ RelationAlias {}) = do
    let def = Persist.entityDef . fmap Persist.entityVal . toProxy $ a
        colNum = Persist.entityColumnCount def
    State.modify' (+ colNum)
    return x
countField i x @ (Star (RelationAliasSub _ a)) = do
    let c = countFields i a
    State.modify' (+ c)
    return x
countField _ x @ Sel {} = do
    State.modify' (+ 1)
    return x

mkPersistConvertEntity :: (Persist.PersistEntity a) => RelationAlias (Persist.Entity a) -> PersistConvert (Persist.Entity a)
mkPersistConvertEntity a = do
    let def = Persist.entityDef . fmap Persist.entityVal . toProxy $ a
        colNum = Persist.entityColumnCount def
    xs <- State.get
    (vals, rest) <- maybe (lift . Left . ConvertError $ "not enough input values") return (splitAtExactMay colNum xs)
    State.put rest
    lift . mapLeft ConvertError $ Persist.parseEntityValues def vals

mkPersistConvertField :: (Persist.PersistField t) => Proxy t -> PersistConvert t
mkPersistConvertField _ = do
    xs <- State.get
    (val, rest) <- maybe (lift . Left . ConvertError $ "not enough input values") return (uncons xs)
    State.put rest
    lift . mapLeft ConvertError . Persist.fromPersistValue $ val

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right c) = Right c

toProxy :: f a -> Proxy a
toProxy _ = Proxy
