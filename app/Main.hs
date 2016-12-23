
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding.UTF7.IMAP as T

import qualified Sgf.Text.IMAPUtf7 as S


main :: IO ()
main                = do
    c <- C.getContents
    let c' = T.unlines . map T.decodeUtf7 . C.lines $ c
    let d' = T.unlines . map (S.decode . C.unpack) . C.lines $ c
    if (c' /= d')
      then putStrLn "Warning, does not match"
      else putStrLn "Matches.."
    T.putStr c'

