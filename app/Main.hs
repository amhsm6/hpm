module Main where

import Control.Monad
import Data.IORef
import System.Environment
import System.Exit
import System.Posix (fileExist, getFileStatus, isDirectory)
import System.Directory
import System.FilePath

config :: FilePath
config = "/etc/hpm"

subdirs :: [FilePath]
subdirs = [ "bin"
          , "lib"
          , "include"
          , "share"
          ]

outdir :: FilePath
outdir = "/usr/local"

copy :: IORef [FilePath] -> FilePath -> FilePath -> IO ()
copy tree src dst = do
    let out = dst </> takeFileName src

    fileExist out >>= \exists -> unless exists $ modifyIORef tree (src:)

    dir <- isDirectory <$> getFileStatus src
    if dir then do
        createDirectoryIfMissing False out
        listDirectory >=> mapM_ (\x -> copy tree (src </> x) out) $ src
    else
        copyFile src out

new :: String -> IO ()
new name = do
    let lock = config </> name
        process subdir = do
            tree <- newIORef []
            copy tree subdir outdir
            readIORef tree >>= appendFile lock . unlines

    fileExist lock >>= \exists -> when exists packageExists
    forM_ subdirs $ \x -> fileExist x >>= \exists -> when exists $ process x

remove :: String -> IO ()
remove name = do
    let lock = config </> name
        rm path = do
            let out = outdir </> path
            dir <- isDirectory <$> getFileStatus out
            if dir then removeDirectory out else removeFile out

    fileExist lock >>= \exists -> unless exists packageNotFound

    readFile lock >>= mapM_ rm . words
    removeFile lock

usage :: IO ()
usage = do
    putStrLn "Usage: hpm <command> <package_name>"
    putStrLn "Available commands:"
    putStrLn "    n        Register new package"
    putStrLn "    r        Remove existing package"
    exitFailure

packageExists :: IO ()
packageExists = do
    putStrLn "Package with this name already exists"
    exitFailure

packageNotFound :: IO ()
packageNotFound = do
    putStrLn "Package with this name is not registered"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then usage
    else case head args of
             "n" -> new $ last args
             "r" -> remove $ last args
             _   -> usage
