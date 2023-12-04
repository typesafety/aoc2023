{-# language LambdaCase #-}
{-# language NoImplicitPrelude #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}

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
    module Data.IntSet,
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

import "base" Prelude as PreludeLess hiding (
    -- Hide functions on lists by default in favor of NonEmpty variants.  Use
    -- List.<function> to use list variants.
    (!!),
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

    -- Hide `id` since it's a useful variable name (redefined as `identity` in
    -- this module).
    id,
 )

import "base" Control.Applicative ((<|>), empty)
import "base" Control.Monad (
    (<=<),
    (>=>),
    foldM,
    forM,
    forM_,
    replicateM_,
    void,
    when,
    )
import "base" Data.Bifunctor
import "base" Data.Char (digitToInt, isDigit)
import "base" Data.Coerce (coerce)
import "base" Data.Foldable (foldl', find)
import "base" Data.Functor ((<&>), ($>), (<$))
import "base" Data.Kind (Type)
import "base" Data.List qualified as List
import "base" Data.List.NonEmpty hiding ((<|))
import "base" Data.Maybe (fromMaybe, isJust)
import "base" Data.Void (Void)
import "base" Debug.Trace (
    trace,
    traceIO,
    traceId,
    traceM,
    traceShow,
    traceShowId,
    traceShowM,
    )
import "base" GHC.Generics (Generic)
import "base" GHC.Stack (HasCallStack)
import "base" System.IO.Unsafe (unsafePerformIO)
import "base" Unsafe.Coerce (unsafeCoerce)

import "containers" Data.IntMap.Strict (IntMap)
import "containers" Data.IntSet (IntSet)
import "containers" Data.Sequence (Seq(..), (<|), (|>))
import "megaparsec" Text.Megaparsec qualified as P
import "megaparsec" Text.Megaparsec.Char qualified as P
import "mtl" Control.Monad.Reader (Reader, ReaderT)
import "mtl" Control.Monad.State.Strict (State, StateT)
import "pretty-simple" Debug.Pretty.Simple (
    pTrace,
    pTraceIO,
    pTraceId,
    pTraceM,
    pTraceShow,
    pTraceShowId,
    pTraceShowM,
    )
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "text" Data.Text.IO qualified as Text
import "text-show" TextShow hiding (singleton)
import "unordered-containers" Data.HashMap.Strict (HashMap)
import "unordered-containers" Data.HashSet (HashSet)

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
