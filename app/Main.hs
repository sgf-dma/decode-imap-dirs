{-# LANGUAGE CPP #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding.UTF7.IMAP as T

#ifdef TestSgfSplit
import qualified Sgf.Text.IMAPUtf7 as S
#endif


main :: IO ()
main                = do
    c <- C.getContents
    let c' = T.unlines . map T.decodeUtf7 . C.lines $ c
#ifdef TestSgfSplit
    let d' = T.unlines . map S.decodeUtf7B . C.lines $ c
    if (c' /= d')
      then putStrLn "Warning, does not match"
      else putStrLn "Matches.."
#endif
    T.putStr c'

