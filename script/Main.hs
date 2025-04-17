module Main where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (for_)
import System.Directory
  ( doesDirectoryExist,
    getCurrentDirectory,
    removeDirectoryRecursive,
    setCurrentDirectory,
  )
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)

nothing :: MaybeT IO a
nothing = MaybeT (pure Nothing)

-- for now. we use MaybeT. Later on, let's use proper exception handling.
runCmd :: FilePath -> [String] -> MaybeT IO ()
runCmd cmd args = do
  (exCode, strStdOut, strStdErr) <- liftIO $ readProcessWithExitCode cmd args ""
  case exCode of
    ExitSuccess -> do
      liftIO $ putStrLn strStdErr
      pure ()
    ExitFailure _ -> do
      liftIO $ putStrLn strStdErr
      nothing

runOn :: FilePath -> MaybeT IO a -> MaybeT IO a
runOn dir action = do
  cwd <- liftIO $ getCurrentDirectory
  liftIO $ setCurrentDirectory dir
  x <- action
  liftIO $ setCurrentDirectory cwd
  pure x

data GerritEnv = GerritEnv
  { gerritServerUrl :: String
  , gerritProject :: String
  , gerritBranch :: String
  , gerritChangeNumber :: String
  , gerritPatchsetNumber :: String
  }
  deriving Show

data RepoInfo = RepoInfo
  { repoName :: String,
    repoBranch :: String,
    repoLocation :: FilePath
  }
  deriving Show

-- | Relative positions of repos
newtype RepoConfig = RepoConfig
  { repoMap :: [RepoInfo]
  }
  deriving Show

repoConfig :: RepoConfig
repoConfig = RepoConfig
  { repoMap =
      [ RepoInfo {repoName = "buck2", repoBranch = "mercury", repoLocation =""}
      , RepoInfo {repoName = "buck2-prelude", repoBranch = "experiments", repoLocation = "buck2-test-suites"}
      , RepoInfo {repoName = "ghc-persistent-worker", repoBranch = "experiments", repoLocation = ""}
      ]
  }

mkRefChange :: GerritEnv -> String
mkRefChange env =
  "refs"
    </> "changes"
    </> truncateFromEnd 2 (gerritChangeNumber env)
    </> gerritChangeNumber env
    </> gerritPatchsetNumber env
  where
    truncateFromEnd n = reverse . take n . reverse

gitCheckout :: GerritEnv -> FilePath -> String -> String -> MaybeT IO ()
gitCheckout env dir repo branch =
  runOn dir $ do
    let repoDir = dir </> repo
        repoUrl = (gerritServerUrl env) </> repo
    liftIO $ do
       b <- doesDirectoryExist repoDir
       when b $ do
         removeDirectoryRecursive repoDir
    runCmd "git" ["clone", repoUrl]
    if (gerritProject env == repo)
      then do
        runOn repoDir $ do
          runCmd "git" ["fetch", "origin", mkRefChange env]
          runCmd "git" ["checkout", "FETCH_HEAD"]
      else do
        runOn repoDir $ do
          runCmd "git" ["checkout", branch]

main :: IO ()
main = do
  putStrLn "Haskell CI Script"
  void $ runMaybeT $ do
    gerritServerUrl <- MaybeT $ lookupEnv "GERRIT_SERVER_URL"
    gerritProject <- MaybeT $ lookupEnv "GERRIT_PROJECT"
    gerritBranch <- MaybeT $ lookupEnv "GERRIT_BRANCH"
    gerritChangeNumber <- MaybeT $ lookupEnv "GERRIT_CHANGE_NUMBER"
    gerritPatchsetNumber <- MaybeT $ lookupEnv "GERRIT_PATCHSET_NUMBER"
    let env = GerritEnv {gerritServerUrl, gerritProject, gerritBranch, gerritChangeNumber, gerritPatchsetNumber}

    cwd <- liftIO getCurrentDirectory
    for_ (repoMap repoConfig) $ \info ->
      gitCheckout env (cwd </> (repoLocation info)) (repoName info) (repoBranch info)
