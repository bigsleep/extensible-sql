{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module ExSql.Syntax.Internal
    ( AFields
    , ARef(..)
    , Field(..)
    , FieldClause(..)
    , FieldsSelector(..)
    , From(..)
    , FromProxy(..)
    , KnownConstructor(..)
    , OrderType(..)
    , PersistConvert
    , RRef(..)
    , Ref(..)
    , RelationAlias(..)
    , ResultType
    , Sel(..)
    , SelWithAlias(..)
    , SelectClause(..)
    , SelectClauses(..)
    , SelectQuery(..)
    , SelectQueryInternal(..)
    , SelectQueryM
    , Selectable(..)
    , ValueList
    , evalSubQuerySel
    , fromId
    , getRelationAlias
    , pattern (:<)
    , pattern Nil
    , relationAliasId
    , rrefId
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as State (evalState,
                                                            evalStateT,
                                                            execState, get,
                                                            mapStateT, modify',
                                                            put, runState)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (mapWriter,
                                                              runWriter)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Extensible.HList as HList (HList(..), htraverse)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.List (uncons)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
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
    type RelationSelType t (a :: *) :: k
    mkPersistConvert :: t (Sel g) a -> PersistConvert (SelectResultType t a)
    mkRefAndFieldClauses :: t (Sel g) a -> (SelectRefType t a, [FieldClause g])
    countFields :: Int -> t (Sel g) a -> Int
    mkRelationSel :: From x t g a -> t (Sel g) a

data RelationAlias a where
    RelationAlias :: (Persist.PersistEntity a) => Int -> RelationAlias (Persist.Entity a)
    RelationAliasSub :: Int -> (Int -> Int) -> PersistConvert a -> RelationAlias a
    RelationAliasNullable :: RelationAlias a -> RelationAlias (Maybe a)

relationAliasId :: RelationAlias a -> Int
relationAliasId (RelationAlias i)         = i
relationAliasId (RelationAliasSub i _ _)  = i
relationAliasId (RelationAliasNullable a) = relationAliasId a

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

data SelectQuery t g a where
    SelectQuery :: Selectable t => SelectQueryInternal s t g a -> SelectQuery t g a

type SelectQueryM g = StateT (Int, Int) (Writer (SelectClauses g))

newtype SelectQueryInternal (s :: SelectStage) (t :: (* -> *) -> k -> *) (g :: * -> *) (a :: k) = SelectQueryInternal
    { unSelectQueryInternal :: SelectQueryM g (t (Sel g) a)
    }

data From x (t :: (* -> *) -> k -> *) (g :: * -> *) (a :: k) where
    FromEntity :: Persist.PersistEntity record => RelationAlias (Persist.Entity record) -> From (Persist.Entity record) t g (RelationSelType t (Persist.Entity record))
    FromSubQuery :: (x ~ SelectResultType t a) => RelationAlias x -> SelectQuery t g a -> From x t g a

data FromProxy x (t :: (* -> *) -> k -> *) (g :: * -> *) (a :: k) where
    FPEntity :: Persist.PersistEntity record => proxy (Persist.Entity record) -> FromProxy (Persist.Entity record) t g (RelationSelType t (Persist.Entity record))
    FPSubQuery :: (x ~ SelectResultType t a) => SelectQuery t g a -> FromProxy x t g a

data SelectClause (g :: * -> *) where
    Fields :: [FieldClause g] -> SelectClause g
    From :: From x t g a -> SelectClause g
    Join :: From x t g a -> SelectClause g
    On :: RRef a -> g Bool -> SelectClause g
    Where :: g Bool -> SelectClause g
    GroupBy :: AFields g xs -> SelectClause g
    OrderBy :: g b -> OrderType -> SelectClause g
    Limit :: Int64 -> SelectClause g
    Offset :: Int64 -> SelectClause g
    Initial :: SelectClause g

newtype SelectClauses g = SelectClauses (DList (SelectClause g))
    deriving (Semigroup, Monoid)

instance Hoist (SelectQuery t) where
    hoist f (SelectQuery a) = SelectQuery (hoist f a)

instance Hoist t => Hoist (SelectQueryInternal s t) where
    hoist f (SelectQueryInternal a) = SelectQueryInternal . fmap (hoist (hoist f)) . State.mapStateT h $ a
        where
        h = Writer.mapWriter $ \(x, SelectClauses w) -> (x, SelectClauses (fmap (hoistSelectClause f) w))

instance Hoist (From x t) where
    hoist _ (FromEntity alias)     = FromEntity alias
    hoist f (FromSubQuery alias a) = FromSubQuery alias (hoist f a)

getRelationAlias :: From x t g a -> RelationAlias x
getRelationAlias (FromEntity alias)     = alias
getRelationAlias (FromSubQuery alias _) = alias

fromId :: From x t g a -> Int
fromId = relationAliasId . getRelationAlias

data OrderType = Asc | Desc deriving (Show, Eq)

data Field (g :: * -> *) a where
    Field :: (Persist.PersistField a) => Ref a -> Field g a
    Column :: (Persist.PersistEntity record) => RRef (Persist.Entity record) -> Persist.EntityField record a -> Field g a

instance Hoist Field where
    hoist _ (Field a)    = Field a
    hoist _ (Column t a) = Column t a

type AFields g xs = HList.HList (Field g) xs

newtype ARef g a = ARef (Field g a)

instance Hoist ARef where
    hoist f (ARef a) = ARef (hoist f a)

hoistSelectClause :: (forall x. m x -> n x) -> SelectClause m -> SelectClause n
hoistSelectClause f (Fields a)    = Fields (map (hoistFieldClause f) a)
hoistSelectClause f (From a)      = From (hoist f a)
hoistSelectClause f (Join a) = Join (hoist f a)
hoistSelectClause f (On ref cond) = On ref (f cond)
hoistSelectClause f (Where a)     = Where (f a)
hoistSelectClause f (GroupBy fs)  = GroupBy . runIdentity . HList.htraverse (Identity . hoist f) $ fs
hoistSelectClause f (OrderBy a t) = OrderBy (f a) t
hoistSelectClause _ (Limit a)     = Limit a
hoistSelectClause _ (Offset a)    = Offset a
hoistSelectClause _ Initial       = Initial

hoistFieldClause :: (forall x. m x -> n x) -> FieldClause m -> FieldClause n
hoistFieldClause f (FieldClause a) = FieldClause (hoist f a)

data FieldsSelector g x where
    Raw :: FieldsSelector g [Persist.PersistValue]
    (:$:) :: (KnownConstructor (ResultType b), ConstructorType (ResultType b) ~ (a -> b)) => (a -> b) -> g a -> FieldsSelector g b
    (:*:) :: (KnownConstructor (ResultType b)) => FieldsSelector g (a -> b) -> g a -> FieldsSelector g b

infixl 4 :$:, :*:

instance Hoist FieldsSelector where
    hoist _ Raw       = Raw
    hoist f (g :$: a) = g :$: f a
    hoist f (s :*: a) = hoist f s :*: f a

instance HTraversable FieldsSelector where
    htraverse _ Raw       = pure Raw
    htraverse f (g :$: a) = (g :$:) <$> f a
    htraverse f (s :*: a) = (:*:) <$> htraverse f s <*> f a

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
    type RelationSelType FieldsSelector a = a

    mkPersistConvert Raw =  do
        xs <- State.get
        State.put mempty
        return xs
    mkPersistConvert (f :$: a) = f <$> mkPersistConvertSel a
    mkPersistConvert (s :*: a) = mkPersistConvert s <*> mkPersistConvertSel a

    mkRefAndFieldClauses x =
        let (ref, xs) = flip State.runState mempty . htraverse putFieldClause . flip State.evalState 0 . htraverse mkSelWithAlias $ x
        in (ref, DList.toList xs)

    countFields n =
        flip State.execState 0 . htraverse (countField n)

    mkRelationSel (FromEntity alias) = id :$: Star alias
    mkRelationSel (FromSubQuery _ a) = evalSubQuerySel a

instance Selectable HList.HList where
    type SelectResultType HList.HList xs = HList.HList Identity xs
    type SelectRefType HList.HList xs = HList.HList Ref xs
    type RelationSelType HList.HList a = '[a]

    mkPersistConvert HList.HNil = return HList.HNil
    mkPersistConvert (HList.HCons x xs) =
        HList.HCons <$> fmap return (mkPersistConvertSel x) <*> mkPersistConvert xs

    mkRefAndFieldClauses x =
        let (ref, xs) = flip State.runState mempty . htraverse putFieldClause . flip State.evalState 0 . htraverse mkSelWithAlias $ x
        in (ref, DList.toList xs)

    countFields n =
        flip State.execState 0 . htraverse (countField n)

    mkRelationSel (FromEntity alias) = Star alias `HList.HCons` HList.HNil
    mkRelationSel (FromSubQuery _ a) = evalSubQuerySel a

evalSubQuerySel :: SelectQuery t g a -> t (Sel g) a
evalSubQuerySel (SelectQuery a) =
    fst . Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQueryInternal $ a

putFieldClause :: (Monad m) => SelWithAlias g a -> StateT (DList (FieldClause g)) m (Ref a)
putFieldClause a = do
    let fc = FieldClause a
    State.modify' (`DList.snoc` fc)
    return . aliasToRef $ a

aliasToRef :: SelWithAlias g a -> Ref a
aliasToRef (Star' a)               = RelationRef (RRef (relationAliasId a))
aliasToRef (Sel' _ (FieldAlias i)) = FieldRef (FRef i)

mkSelWithAlias :: Sel g a -> State Int (SelWithAlias g a)
mkSelWithAlias (Star a) = return (Star' a)
mkSelWithAlias (Sel a) = do
    i <- State.get
    State.put (i + 1)
    return (Sel' a (FieldAlias i))

countField :: Int -> Sel g a -> State Int (Sel g a)
countField _ x @ (Star a @ RelationAlias {}) = do
    let def = Persist.entityDef . fmap Persist.entityVal . toProxy $ a
        colNum = Persist.entityColumnCount def
    State.modify' (+ colNum)
    return x
countField i x @ (Star (RelationAliasSub _ c _)) = do
    State.modify' (+ c i)
    return x
countField i x @ (Star (RelationAliasNullable a)) = countField i (Star a) >> return x
countField _ x @ Sel {} = do
    State.modify' (+ 1)
    return x

mkPersistConvertSel :: Sel g a -> PersistConvert a
mkPersistConvertSel (Star a @ RelationAlias {}) = mkPersistConvertEntity a
mkPersistConvertSel (Star (RelationAliasSub _ _ convert)) = convert
mkPersistConvertSel b @ (Star (RelationAliasNullable a)) = do
    xs <- State.get
    let l = length xs
        c = State.execState (countField l b) 0
    State.mapStateT (handleNullable xs c) (mkPersistConvertSel (Star a))

    where
    handleNullable _ _ (Right (x, s)) = Right (Just x, s)
    handleNullable vs c (Left HitNullValue) = do
        (_, rest) <- maybe (Left . ConvertError $ "not enough input values") return (splitAtExactMay c vs)
        return (Nothing, rest)
    handleNullable _ _ (Left (ConvertError e)) = Left (ConvertError e)
mkPersistConvertSel Sel {} = mkPersistConvertField Proxy

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
