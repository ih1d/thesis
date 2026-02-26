import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths
import System.Process (callProcess)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { preBuild = bnfcBuild
    }

bnfcBuild :: Args -> BuildFlags -> IO HookedBuildInfo
bnfcBuild _ _ = do
    createDirectoryIfMissing True "src/MPL"
    callProcess "bnfc"
        [ "--haskell"
        , "-d"
        , "-o", "src"
        , "MPL.cf"
        ]
    return emptyHookedBuildInfo
