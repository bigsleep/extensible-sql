{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, RankNTypes, TypeOperators #-}
module ExSql.Syntax.ClassSpec
    ( spec
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Extensible (Member, Match(..), (:|)(..), (:*), (<:), nil, hindex)
import Data.Functor.Identity (Identity(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Literal
import ExSql.Syntax.Class
import Test.Hspec

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer { runPrinter :: forall a. v g a -> String }

type Nodes = '[Literal, Arithmetic]
type E = Expr Nodes Identity
type Printers xs a = Printer (Expr xs Identity) :* xs

ppArithmetic :: (forall b. Expr xs Identity b -> String) -> Arithmetic (Expr xs Identity) a -> String
ppArithmetic p (Negation a) = "(-" `mappend` p a `mappend` ")"
ppArithmetic p (Addition a0 a1) = "(" `mappend` p a0 `mappend` "+" `mappend` p a1 `mappend` ")"
ppArithmetic p (Subtraction a0 a1) = "(" `mappend` p a0 `mappend` "-" `mappend` p a1 `mappend` ")"
ppArithmetic p (Multiplication a0 a1) = "(" `mappend` p a0 `mappend` "*" `mappend` p a1 `mappend` ")"
ppArithmetic p (Division a0 a1) = "(" `mappend` p a0 `mappend` "/" `mappend` p a1 `mappend` ")"

ppLiteral :: Literal (Expr xs Identity) a -> String
ppLiteral (LitInt a) = show a
ppLiteral (LitBool a) = show a

printers :: (forall b. Expr Nodes Identity b -> String) -> Printers Nodes a
printers p = Printer (ppLiteral)
    <: Printer (ppArithmetic p)
    <: nil

runPrinters :: Printer (Expr xs Identity) :* xs -> Expr xs Identity a -> String
runPrinters printers (Expr (EmbedAt membership (Node (Identity a)))) = runPrinter (hindex printers membership) a

pp :: Expr Nodes Identity a -> String
pp = runPrinters (printers pp)

refInt :: (Ast g, MonadReader Int m, Member (NodeTypes g) Literal) => g m Int
refInt = mkAst (ask >>= return . LitInt)

refMap :: (Ast g, MonadReader (Map String Int) m, Member (NodeTypes g) Literal) => String -> g m Int
refMap name = mkAst (ask >>= return . LitInt . Map.findWithDefault 0 name)

e1 :: (Ast g, MonadReader Int m,  Member (NodeTypes g) Arithmetic, Member (NodeTypes g) Literal) => g m Int
e1 = addition refInt refInt

e2 :: (Ast g, MonadReader (Map String Int) m,  Member (NodeTypes g) Arithmetic, Member (NodeTypes g) Literal) => g m Int
e2 = addition (refMap "a") (refMap "b")

spec :: Spec
spec = do
    describe "hoistExpr" $ do
        it "can use with MonadReader" $ do
            pp (hoistExpr (flip runReaderT 1) e1) `shouldBe` "(1+1)"

        it "with Map" $ do
            let m = Map.fromList [("a", 123), ("b", 456)]
            pp (hoistExpr (flip runReaderT m) e2) `shouldBe` "(123+456)"
