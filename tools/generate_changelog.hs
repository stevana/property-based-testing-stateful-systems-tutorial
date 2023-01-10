#!/usr/bin/env runghc

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Map           (Map)
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import           System.Environment
import           System.Exit
import           System.Process

------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 1 || length args == 2) printHelp
  let fromTag = args !! 0
      toTag   | length args == 1 = "HEAD"
              | otherwise        = args !! 1
  gitLog <- readProcess "git"
    [ "log"
    , fromTag ++ ".." ++ toTag
    , "--no-merges"
    , "--pretty=format:%s ([%h](" ++ repoUrl ++ "/commit/%H))"
    ]
    ""
  date <- readProcess "date" ["+%Y-%m-%d"] ""
  putStrLn (concat
             [ "## [", toTag, "](", repoUrl, "/compare/", fromTag, "...", toTag, ")"
             , " (" ++ dropWhileEnd isSpace date ++ ")"])
  putStrLn ""
  putStrLn (prettyBucket (createBucket (lines gitLog)))

repoUrl :: String
repoUrl = "https://github.com/stevana/property-based-testing-stateful-systems-tutorial"

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  putStrLn (progName ++ ": <from-rev> [<to-rev>]")
  exitFailure

------------------------------------------------------------------------

type Bucket = Map String [String]

createBucket :: [String] -> Bucket
createBucket = foldl insertBucket Map.empty

knownCategories :: Map String String
knownCategories = Map.fromList
  [ ("test",      "Testing improvements")
  , ("feat",      "New features")
  , ("add",       "New features")
  , ("fix",       "Bug fixes")
  , ("bug",       "Bug fixes")
  , ("perf",      "Performance improvements")
  , ("pref",      "Performance improvements")
  , ("roadmap",   "Documentation improvements")
  , ("readme",    "Documentation improvements")
  , ("docs",      "Documentation improvements")
  , ("doc",       "Documentation improvements")
  , ("changelog", "Documentation improvements")
  , ("nix",       "Build improvements")
  , ("bazel",     "Build improvements")
  , ("build",     "Build improvements")
  , ("remove",    "Removed features")
  , ("cleanup",   "Removed features")
  , ("refactor",  "Refactorings")
  , ("ci",        "CI improvements")
  ]

insertBucket :: Bucket -> String -> Bucket
insertBucket bucket line =
  let
    (tag, _colon : message) = break (== ':') line
    (category, context) = span (/= '(') tag
    message' = dropWhile isSpace message
    context' = "**" ++ dropWhileEnd (==')') (dropWhile (=='(') context) ++ "**: "
    context'' | context' == "****: " = ""
              | otherwise            = context'
  in
    if category `Set.member` Map.keysSet knownCategories
    then
      Map.insertWith (++) (knownCategories Map.! category)
        [ "* " ++ context'' ++ message' ] bucket
    else
      Map.insertWith (++) "Uncategorised" [ "* " ++ line ] bucket

prettyBucket :: Bucket -> String
prettyBucket bucket = unlines
  [ unlines (("### " ++ category) : reverse items)
  | (category, items) <- Map.toList bucket
  ]
