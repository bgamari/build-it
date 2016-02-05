import Data.Monoid
import System.Directory
import Control.Monad.IO.Class
import Data.List (isSuffixOf)

import Build

threads = 4

git :: [String] -> StepM ()
git = run "git"

make :: [String] -> StepM ()
make = run "make"

updateRepo :: Step ()
updateRepo = step "update" $ do
    git ["pull"]
    git ["submodule", "update", "--init"]

getCommit :: Step ()
getCommit = step "get-commit" $ git ["rev-parse", "HEAD"]

checkBootstrap :: Step ()
checkBootstrap = step "check-bootstrap" $ run "ghc" ["--info"]

cleanRepo :: Step ()
cleanRepo = step "clean" $ make ["distclean"]

configure :: Step ()
configure = step "configure" $ do
    run "./boot" []
    run "./configure" []

compile :: Step ()
compile = step "compile" $ make ["-j"<>show threads]

binDist :: Step ()
binDist = step "bindist" $ do
    make ["binary-dist"]
    f:_ <- filter (".tar.xz" `isSuffixOf`) <$> liftIO (getDirectoryContents ".")
    copyArtifact (ArtifactName "bindist") f

testBinDist :: Step ()
testBinDist = step "test-bindist" $ make ["test_bindist"]

testsuite :: Step ()
testsuite = step "test" $ make ["test", "THREADS="<>show threads]

ghcBuild :: Build
ghcBuild = buildSteps
    [ cleanRepo
    , updateRepo
    , getCommit
    , checkBootstrap
    , configure
    , compile
    , binDist
    , testBinDist
    , testsuite
    ]

main :: IO ()
main = do
    env <- simpleBuildEnv
    runAndPackageBuild env ghcBuild >>= print

