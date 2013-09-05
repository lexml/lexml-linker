import Distribution.Simple
import System.Process 

main = defaultMainWithHooks $ simpleUserHooks {
      preBuild = \args buildFlags -> (makeVersionFile >> preBuild simpleUserHooks args buildFlags)
  }

makeVersionFile = do
  runCommand "sh src/main/bash/makeversion.sh"
--  writeFile "src/main/haskell/LexML/Version.hs" "module LexML.Version where\n version = \"2326\""
