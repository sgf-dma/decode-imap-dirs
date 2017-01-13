{-# LANGUAGE CPP #-}

import qualified Data.ByteString.Char8          as C
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T
import qualified Data.Text.Encoding.UTF7.IMAP   as T

#ifdef TestSgfSplit
import Control.Monad
import System.IO (hPutStrLn, stderr)
import qualified Sgf.Text.IMAPUtf7 as S
#endif


main :: IO ()
main                = do
    l <- C.getContents
    let cs = C.lines l
        xs = map T.decodeUtf7 cs
#ifdef TestSgfSplit
    let ys = map S.decodeUtf7B cs
        zs = filter (uncurry (/=)) $ zip xs ys
    forM_ zs $ \(x, y) -> hPutStrLn stderr $ "Results do not match:\n"
      ++ "text-utf7: '" ++ T.unpack x ++ "'\n"
      ++ "mine: '"    ++ T.unpack y ++ "'\n"
#endif
    let inLine c x = T.decodeUtf8 c `T.append` T.pack "|" `T.append` x
    T.putStr . T.unlines $ zipWith inLine cs xs

