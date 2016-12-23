
module Sgf.Text.IMAPUtf7
    ( IMAPString (..)
    , append
    , split
    , decode
    )
  where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Encoding.UTF7.IMAP as T
import qualified Data.ByteString.Char8 as C


-- Generalized `groupBy`.
groupByA :: Alternative f => (a -> a -> Bool) -> [a] -> [f a]
groupByA _ []       = []
groupByA eq ts      = snd $ foldr (\x z -> (x, go x z)) (last ts, []) ts
  where
    -- For last list element i provide dummy next element equal to it.
    --go :: a -> (a, [[a]]) -> [[a]]
    go x (y, ws)
      | x `eq` y      = case ws of
                        []       ->  pure x         : []
                        (z : zs) -> (pure x <|> z)  : zs
      | otherwise   =                pure x         : ws

-- Another generalization of `groupBy`. If user-supplied function is able to
-- merge two elements, it should return `Just` with merged element, otherwise
-- it should return `Nothing` and i will keep original element.
groupBy' :: (a -> a -> Maybe a) -> [a] -> [a]
groupBy' _ []       = []
groupBy' f ts       = let (zs, mw) = foldr go ([], Nothing) ts
                      in  maybe zs (: zs) mw
  where
    --go :: a -> ([a], Maybe a) -> ([a], Maybe a)
    go x (zs, mw)   = case (mw >>= f x) of
                        Just y  -> (zs                  , Just y)
                        Nothing -> (maybe zs (: zs) mw  , Just x)


-- | IMAP Utf7 string may contain either @ascii@ or @utf7@ characters.
data IMAPString     = Plain {imapString :: String}
                    | Utf7  {imapString :: String}
  deriving (Show)

-- | Merge two 'IMAPString' values.
append :: IMAPString -> IMAPString -> Maybe IMAPString
append (Plain xs) (Plain ys)  = Just (Plain (xs ++ ys))
append (Utf7  xs) (Utf7  ys)  = Just (Utf7  (xs ++ ys))
append _          _           = Nothing

-- | Split string to ascii and utf7 portions.
split' :: String -> [IMAPString]
split' []         = []
split' ts         = foldr (\x f y m -> go x (f x) y m) (\_ m -> [m]) ts '.' (Plain [])
  where
    -- Pass mode (using empty string with correct constructor) to the
    -- accumulator and only *then* pattern match on its result.
    go :: Char -> (IMAPString -> [IMAPString]) -> Char -> IMAPString -> [IMAPString]
    go x zs y m
      | y == '&' && x == '-'    = case m of
                                    Plain _ -> Plain ['&']  : (zs (Plain []))
                                    Utf7  _ -> error "No parse 1."
      | y == '&'                = case m of
                                    Plain _ -> Utf7  [x]    : (zs (Utf7 []))
                                    Utf7  _ -> error "No parse 2."
      |             x == '&'    = zs m
      |             x == '-'    = case m of
                                    Plain _ -> Plain ['-']  : (zs m)
                                    Utf7  _ -> zs (Plain [])
      | otherwise               = m{imapString = [x]} : (zs m)

split :: String -> [IMAPString]
split             = groupBy' append . split'

decode :: String -> T.Text
decode            = T.concat . map go . split
  where
    go :: IMAPString -> T.Text
    go (Plain xs)   = T.pack xs
    go (Utf7  xs)   = T.decodeUtf7 (C.pack ('&' : (xs ++ "-")))

