-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import SOLTest.Types
import System.Directory
  ( doesFileExist,
    listDirectory,
    doesDirectoryExist
  )
import System.FilePath (replaceExtension, takeBaseName, (</>), takeExtension)
import Control.Monad (filterM)

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
discoverTests :: Bool -> FilePath -> IO [TestCaseFile]
discoverTests recursive dir = do
  -- get names of all files in directory and then convert them to full paths
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  
  -- filter .test files from all files in directory
  let testPaths = filter (\path -> takeExtension path == ".test") fullPaths

  -- create TestCaseFile list from them
  testFiles <- mapM findCompanionFiles testPaths

  if not recursive
  then
    return testFiles
  else do
    -- get subdirectories in current dir
    dirs <- filterM doesDirectoryExist fullPaths
    -- recursively call this function again for each subdirectory
    dir_tests <- mapM (discoverTests True) dirs
    -- concatenate list of .test files from this dir and from its subdirectories
    return $ testFiles ++ concat dir_tests



-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }
