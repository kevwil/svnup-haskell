-- Copyright (c) 2007 Kevin D Williams
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

module Main where

import Control.Monad (filterM)
import System.Cmd (rawSystem)
import System.Directory (doesDirectoryExist,getDirectoryContents)
import System.Environment (getArgs)
import System.Exit

main :: IO ()
main = do
  -- capture single arg as a String
  [basedir] <- getArgs
  -- verification
  dirTest <- doesDirectoryExist basedir
  if dirTest
    then do
      subdirs <- getDirectoryContents basedir
      -- filter subdirectories to only those who
      -- are Subversion-enabled workspaces.
      -- First runs fullPaths over the subdirs array,
      -- then runs the hasMeta filter.
      workspaces <- filterM hasMeta ( fullPaths basedir subdirs )
      -- iterate over those directories and perform update
      mapM_ runUpdate workspaces
      exitSuccess
    else 
      putStrLn "not a directory"

-- get directory contents as full paths
fullPaths :: FilePath -> [FilePath] -> [FilePath]
fullPaths basepath =
  map (\x -> basepath ++ "/" ++ x)
  -- apply lambda expression to list, concatenating
  -- base path in front of each path

-- check for .svn metadata directory
hasMeta :: FilePath -> IO Bool
hasMeta ( someDir ) =
  doesDirectoryExist( someDir ++ "/.svn" )

-- run 'svn up' on workspace directory
runUpdate :: FilePath -> IO Bool
runUpdate targetDir = do
  putStrLn ("#### updating " ++ targetDir ++ " ####")
  -- execute external system call
  rawSystem "svn" ["up",targetDir]
  return True
