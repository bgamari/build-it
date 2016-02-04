import Data.Monoid

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
getCommit = step "commit" $ return ()

cleanRepo :: Step ()
cleanRepo = step "clean" $ make ["distclean"]

configure :: Step ()
configure = step "configure" $ do
    run "./boot" []
    run "./configure" []

compile :: Step ()
compile = step "compile" $ make ["-j"<>show threads]

binDist :: Step ()
binDist = step "bindist" $ make ["binary-dist"]

testBinDist :: Step ()
testBinDist = step "test-bindist" $ make ["test_bindist"]

testsuite :: Step ()
testsuite = step "test" $ make ["test", "THREADS="<>show threads]

ghcBuild :: Build
ghcBuild = buildSteps
    [ cleanRepo
    , updateRepo
    , configure
    , compile
    , binDist
    , testBinDist
    , testsuite
    ]

main :: IO ()
main = do
    env <- simpleBuildEnv
    let env' = env { buildCwd   = "/opt/exp/ghc/ghc-landing" }
    runBuild env' ghcBuild

