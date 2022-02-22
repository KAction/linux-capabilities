module Main where
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Control.Lens
import System.Xattr (setxattr, XattrMode(..))
import Data.Text (pack)
import qualified System.Capability as Capability
import System.Capability (permitted, inheritable)

parseSet :: String -> Capability.Set
parseSet s =
  let names = splitOn "," s
      caps = catMaybes $ map (flip lookup Capability.known . pack) names
  in mconcat caps

main :: IO ()
main = do
  [filename, plist, ilist] <- getArgs
  let plist' = parseSet plist
      ilist' = parseSet ilist
  putStrLn $ show (plist', ilist')
  let f = mempty & permitted .~ plist' & inheritable .~ ilist'
  setxattr filename "security.capability" (Capability.encode f) RegularMode
