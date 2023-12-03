{- | Alternative prelude.

Add imports here for things used often.
-}
module MyPrelude (
    module PreludeLess,
    module MyPrelude,

    module Control.Applicative,
    module Control.Monad,
    module Control.Monad.Reader,
    module Control.Monad.State.Strict,
    module Data.Bifunctor,
    module Data.Char,
    module Data.Coerce,
    module Data.Functor,
    module Data.Foldable,
    module Data.HashMap.Strict,
    module Data.HashSet,
    module Data.IntMap.Strict,
    module Data.Kind,
    module Data.List.NonEmpty,
    module Data.Maybe,
    module Data.Sequence,
    module Data.Text,
    module Data.Void,
    module Debug.Pretty.Simple,
    module Debug.Trace,
    module GHC.Generics,
    module System.IO.Unsafe,
    module TextShow,
) where

import Prelude as PreludeLess hiding (
    -- Hide functions on lists by default in favor of NonEmpty variants.  Use
    -- List.<function> to use list variants.
    break,
    cycle,
    drop,
    dropWhile,
    filter,
    head,
    init,
    iterate,
    last,
    length,
    map,
    repeat,
    reverse,
    scanl,
    scanl1,
    scanr,
    scanr1,
    span,
    splitAt,
    tail,
    take,
    takeWhile,
    unzip,
    zip,
    zipWith,
    (!!),

    -- Hide `id` since it's a useful variable name (redefined as `identity` in
    -- this module).
    id,
 )

import Control.Applicative ((<|>), empty)
import Control.Monad ((>=>), (<=<), when, forM_, forM,void, replicateM_)
import Control.Monad.Reader (Reader, ReaderT)
import Control.Monad.State.Strict (State, StateT)
import Data.Bifunctor
import Data.Char (digitToInt, isDigit)
import Data.Coerce (coerce)
import Data.Foldable (foldl', find)
import Data.Functor ((<&>), ($>), (<$))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.IntMap.Strict (IntMap)
import Data.Kind (Type)
import Data.List qualified as List
import Data.List.NonEmpty hiding ((<|))
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq(..), (<|), (|>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void (Void)
import Debug.Trace (
    trace,
    traceIO,
    traceId,
    traceM,
    traceShow,
    traceShowId,
    traceShowM,
 )
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Debug.Pretty.Simple (
    pTrace,
    pTraceIO,
    pTraceId,
    pTraceM,
    pTraceShow,
    pTraceShowId,
    pTraceShowM,
 )
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import TextShow hiding (singleton)


-- * Parsing and plumbing

partialParse :: forall s b . (HasCallStack, P.Stream s, Show s) => P.Parsec Void s b -> s -> b
partialParse parser input =
    fromMaybe (error $ "partialParseText: failed to parse input: " <> show input)
    $ P.parseMaybe @Void @s @b parser input

partialParseString :: P.Parsec Void String Int -> String -> Int
partialParseString = partialParse

partialParseText :: P.Parsec Void Text Int -> Text -> Int
partialParseText = partialParse

partialNonEmpty :: HasCallStack => [a] -> NonEmpty a
partialNonEmpty xs = case nonEmpty xs of
    Just ys -> ys
    Nothing -> error "partialNonEmpty: empty list"

partialFromJust :: HasCallStack => Maybe a -> a
partialFromJust = \case
    Just x -> x
    Nothing -> error "partialFromJust: Nothing"

partialHead :: HasCallStack => [a] -> a
partialHead = \case
    [] -> error "partialHead: empty list"
    xs -> List.head xs

partialLast :: HasCallStack => [a] -> a
partialLast = \case
    [] -> error "partialLast: empty list"
    xs -> List.last xs

-- ** Extra combinators

-- | Return True when the end of the line has been reached.  Consumes input.
atEol :: (P.MonadParsec e s m, P.Token s ~ Char) => m Bool
atEol = (P.eol $> True) <|> pure False

-- * Working with Text

putTextLn :: Text -> IO ()
putTextLn = Text.putStrLn

-- | Go from a showable type to 'Text' the really slow way, via 'String'.
showt' :: Show a => a -> Text
showt' = Text.pack . show

packt :: String -> Text
packt = Text.pack

unpackt :: Text -> String
unpackt = Text.unpack

-- * Bifunctor

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

-- * Misc

-- | Identity function.
identity :: a -> a
identity x = x

-- | Apply a function N times.
applyN :: Int -> (a -> a) -> a -> a
applyN n f = List.foldl' (.) identity (replicate n f)

-- | n - 1
decr :: Integral a => a -> a
decr n = n - 1

-- * Missing functor operators

(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f <.> g = fmap f . g

-- * Placeholders

{-# WARNING todo "Unhandled TODO placeholder expression." #-}
todo :: HasCallStack => a
todo = error "ERROR: TODO IN CODE"

{-# WARNING correct "Use of correct/unsafeCoerce in code." #-}
correct :: HasCallStack => a -> b
correct = unsafeCoerce
