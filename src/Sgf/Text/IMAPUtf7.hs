
module Sgf.Text.IMAPUtf7
    ( -- $main

      -- * List operations.
      --
      -- $list
      groupByA
    , groupByF

    -- * Types.
    --
    -- $imapstring
    , IMAPString (..)
    , append

    -- * Split.
    --
    -- $split
    , splitF
    , split
    , splitB

    -- * Decode.
    --
    -- $decode
    , decodeUtf7
    , decodeUtf7B
    )
  where

import Control.Monad
import Control.Arrow
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding.UTF7.IMAP as T
import qualified Data.ByteString.Char8 as C

import Sgf.Data.Fold


-- $main
--
-- This is an implementation of split of imap utf7 string to ascii and utf7
-- parts. For actual decoding i still use 'decodeUtf7'. I.e. this is just a
-- useless piece of complete imap utf7 decoding, which is already implemented
-- either way.


-- List operations.
-- $list

-- | Generalized 'Data.List.groupBy'.
groupByA :: Alternative f => (a -> a -> Bool) -> [a] -> [f a]
groupByA _ []       = []
groupByA eq ts      = snd $ foldr (\x z -> (x, go x z)) (last ts, []) ts
  where
    -- For last list element i provide dummy next element equal to it.
    --go :: a -> (a, [[a]]) -> [[a]]
    go x (y, ws)
      | x `eq` y    = case ws of
                        []       -> [pure x]
                        (z : zs) -> (pure x <|> z)  : zs
      | otherwise   =                pure x         : ws

-- | Another generalization of 'Data.List.groupBy' working over any list-like
-- data type supported by 'Fold'. If user-supplied function is able to merge
-- two elements, it should return 'Just' with merged element, otherwise it
-- should return 'Nothing' and the function will keep original element.
--
-- User-supplied function /must/ be associative for this to work properly!
-- Because 'Maybe' essentially adds zero element for some multiplication
-- operation @(g :: a -> a -> a)@ . I.e.
--
-- >    0 `f` x === x `f` 0 === 0
--
-- for any @(x :: a)@, where @(f :: a -> a -> Maybe a)@ .
--
-- In this 'groupBy'' i assume, that @f@ is associative. I.e.
--
-- >    y `f` (x `f` z) === (y `f` x) `f` z
--
-- then
--
-- >    y `f` x == 0    ==>     y `f` (x `f` z) == 0
--
-- for any @(z :: a)@.
--
-- Thus, to determine whether i will be able to add preceding element @y@ to
-- the current product @(x \`f\` z)@, i don't need to know entire product @z@, but
-- just current element @x@ and preceding element @y@. And this makes possible
-- to write 'groupBy'' working on infinity lists.
groupByF :: (a -> a -> Maybe a) -> Fold a [a]
--groupBy' :: (a -> a -> Maybe a) -> [a] -> [a]
--groupBy' _ []       = []
--groupBy' f ts       = merge $ foldr go (\my -> ([], my)) ts Nothing
groupByF f         = Fold go (\my -> ([], my)) (merge . ($ Nothing))
  where
    --go :: a -> (Maybe a -> ([a], Maybe a)) -> Maybe a -> ([a], Maybe a)
    go x h my       = let mz = my >>= flip f x
                      in  case mz of
                            Just _  -> second (join . liftM2 f my) $ h (Just x)
                            Nothing -> (merge (h (Just x)), my)
    merge :: ([a], Maybe a) -> [a]
    merge (zs, mz)  = maybe zs (: zs) mz


-- $imapstring

-- | IMAP Utf7 string may contain either @ascii@ or @utf7@ characters.
data IMAPString     = Plain {imapString :: String}  -- ^ ASCII portion.
                    | Utf7  {imapString :: String}  -- ^ UTF7  portion.
  deriving (Show)

-- | Merge two 'IMAPString' values, if they have the same constructor.
append :: IMAPString -> IMAPString -> Maybe IMAPString
append (Plain xs) (Plain ys)  = Just (Plain (xs ++ ys))
append (Utf7  xs) (Utf7  ys)  = Just (Utf7  (xs ++ ys))
append _          _           = Nothing


-- $split

-- | Split string to ascii and utf7 portions. This is generic version, which
-- does /not/ 'append' 'IMAPString` elements. I.e. each input character is
-- converted (if possible) to one 'IMAPString' value, making 'splitF' able to
-- work on infinity lists: in case the last required element is @&@, it will
-- require the following element to be defined too.
splitF :: Fold Char [IMAPString]
--split' :: String -> [IMAPString]
--split' []           = []
--split' ts           = foldr (\x f y m -> go x (f x) y m) (\_ m -> [m]) ts '.' (Plain [])
splitF              = Fold  (\x f y m -> go x (f x) y m) (\_ m -> [m])
                            (\h -> h '.' (Plain []))
  where
    -- Pass mode (using empty string with correct constructor) to the
    -- accumulator and only *then* pattern match on its result.
    go :: Char -> (IMAPString -> [IMAPString]) -> Char -> IMAPString -> [IMAPString]
    go x zs y m
      | y == '&' && x == '-'    = case m of
                                    Plain _ -> Plain ['&']  : zs (Plain [])
                                    Utf7  _ -> error "No parse 1."
      | y == '&'                = case m of
                                    Plain _ -> Utf7  [x]    : zs (Utf7 [])
                                    Utf7  _ -> error "No parse 2."
      |             x == '&'    = zs m
      |             x == '-'    = case m of
                                    Plain _ -> Plain ['-']  : zs m
                                    Utf7  _ -> zs (Plain [])
      | otherwise               = m{imapString = [x]} : zs m

-- | Split string to ascii and utf7 portions 'append'-ing adjacent
-- 'IMAPString' values, if possible. Unlike 'splitF', it requires the last
-- evaluated contiguous input part (ascii or utf7), which will be a single
-- 'IMAPString' value now, to be completely defined and at least one defined
-- element from next input part in order to work on infinity lists. Works on
-- 'Foldable' input.
split :: Foldable t => t Char -> [IMAPString]
split               = foldrF (groupByF append) . foldrF splitF

-- | Split string to ascii and utf7 portions 'append'-ing adjacent
-- 'IMAPString' values, if possible. The same as 'split', but works on
-- 'Data.ByteString.Char8.ByteString' input.
splitB :: C.ByteString -> [IMAPString]
splitB              = foldrF (groupByF append) . foldrB splitF


-- $decode

-- | Decode imap utf7 string, using my split functions for splitting input to
-- ascii and utf7 portions, but then just call
-- `Data.Text.Encoding.UTF7.IMAP.decodeUtf7' for actual utf7 decoding.
-- Version for 'Foldable'.
decodeUtf7 :: Foldable t => t Char -> T.Text
decodeUtf7         = T.concat . map go . split
  where
    go :: IMAPString -> T.Text
    go (Plain xs)   = T.pack xs
    go (Utf7  xs)   = T.decodeUtf7 (C.pack ('&' : (xs ++ "-")))

-- | Decode imap utf7 string, using my split functions for splitting input to
-- ascii and utf7 portions, but then just call
-- `Data.Text.Encoding.UTF7.IMAP.decodeUtf7' for actual utf7 decoding.
-- Version for 'Data.ByteString.Char8.ByteString'.
decodeUtf7B :: C.ByteString -> T.Text
decodeUtf7B         = T.concat . map go . splitB
  where
    go :: IMAPString -> T.Text
    go (Plain xs)   = T.pack xs
    go (Utf7  xs)   = T.decodeUtf7 (C.pack ('&' : (xs ++ "-")))

