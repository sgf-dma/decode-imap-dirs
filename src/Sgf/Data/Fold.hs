{-# LANGUAGE GADTs #-}

module Sgf.Data.Fold
    (
    -- $main

    -- * Types.
    --
    -- $types
      Fold (..)

    -- * Folds.
    --
    -- $folds
    , foldrF
    , foldrB
    , foldrT
    )
  where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Control.Arrow


-- $main
--
-- Objectify folds.  See <https://stackoverflow.com/a/12863602>
-- (<https://stackoverflow.com/questions/12857615/making-a-single-function-work-on-lists-bytestrings-and-texts-and-perhaps-other>) .
--
-- I may use this to abstract folding operations over different list-like
-- structures, like 'Foldable', 'Data.ByteString.ByteString' and
-- 'Data.Text.Text'.

-- $types

-- | Represent fold not as operation, but as value consisting from folding
-- function, accumulator of some type (existential) and post-process function.
-- The latter is needed for making `Fold` to be an instance of `Functor`,
-- `Applicative`, etc.
data Fold a c where
    Fold :: { foldingF  :: a -> b -> b      -- ^ Folding function.
            , accF      :: b                -- ^ Accumulator.
            , postF     :: b -> c           -- ^ Post-process result.
            } -> Fold a c

instance Functor (Fold a) where
    fmap f (Fold g z h) = Fold g z (f . h)
instance Applicative (Fold a) where
    pure x          = Fold undefined undefined (const x)
    (Fold g z0 f) <*> (Fold h w0 k)
                    = Fold (\x (z, w) -> (g x z, h x w)) (z0, w0)
                           (uncurry ($) . (f *** k))

-- $folds
--
foldrF :: Foldable t => Fold a c -> t a -> c
foldrF (Fold go z h)    = h . foldr go z

foldrB :: Fold Char c -> C.ByteString -> c
foldrB (Fold go z h)    = h . C.foldr go z

foldrT :: Fold Char c -> T.Text -> c
foldrT (Fold go z h)    = h . T.foldr go z

