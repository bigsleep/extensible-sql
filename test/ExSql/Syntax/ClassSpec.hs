{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, RankNTypes, TypeOperators #-}
module ExSql.Syntax.ClassSpec
    ( spec
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Extensible (Member, Match(..), (:|)(..), (:*), (<:), nil, hindex)
import Data.Functor.Identity (Identity(..))
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

e1 :: (Ast g, MonadReader Int m,  Member (NodeTypes g) Arithmetic, Member (NodeTypes g) Literal) => g m Int
e1 = addition refInt refInt

spec :: Spec
spec = do
    describe "hoistExpr" $ do
        it "can use with MonadReader" $ do
            pp (hoistExpr (flip runReaderT 1) e1) `shouldBe` "(1+1)"
