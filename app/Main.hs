{-# LANGUAGE CPP #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding.UTF7.IMAP as T

#ifdef TestSgfSplit
import Control.Monad
import System.IO (hPutStrLn, stderr)
import qualified Sgf.Text.IMAPUtf7 as S
#endif


main :: IO ()
main                = do
    c <- C.getContents
    let xs = map T.decodeUtf7  . C.lines $ c
#ifdef TestSgfSplit
    let ys = map S.decodeUtf7B . C.lines $ c
        zs = filter (uncurry (/=)) $ zip xs ys
    forM_ zs $ \(x, y) -> hPutStrLn stderr $ "Results do not match:\n"
      ++ "text-utf7: '" ++ T.unpack x ++ "'\n"
      ++ "mine: '"    ++ T.unpack y ++ "'\n"
#endif
    T.putStr (T.unlines xs)

