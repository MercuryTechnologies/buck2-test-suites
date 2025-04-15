module Main where

import System.Environment (lookupEnv)

lookupEnv' :: String -> IO (String, Maybe String)
lookupEnv' name = (name,) <$> lookupEnv name

printEnv :: (String, Maybe String) -> IO ()
printEnv (name, mVal) =
  putStrLn $ "Env Var: " ++ name ++ " = " ++ show mVal

main :: IO ()
main = do
  putStrLn "Haskell CI Script"
  gerritProject <- lookupEnv' "GERRIT_PROJECT"
  gerritBranch <- lookupEnv' "GERRIT_BRANCH"
  gerritChangeNumber <- lookupEnv' "GERRIT_CHANGE_NUMBER"
  gerritPatchsetNumber <- lookupEnv' "GERRIT_PATCHSET_NUMBER"

  printEnv gerritProject
  printEnv gerritBranch
  printEnv gerritChangeNumber
  printEnv gerritPatchsetNumber
