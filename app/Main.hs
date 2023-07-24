{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad
import System.Environment
import System.Exit
import System.Posix (fileExist, getFileStatus, isDirectory)
import System.Directory
import System.FilePath
import GHC.Generics
import Data.Binary

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

data Tree = Node FilePath [Tree] | Leaf FilePath
          deriving (Generic, Show)

instance Binary Tree

build :: FilePath -> IO Tree
build path = do
    let filename = takeFileName path

    dir <- isDirectory <$> getFileStatus path
    if dir then
        listDirectory path >>= mapM (build . (path</>)) >>= pure . Node filename
    else
        pure $ Leaf filename

cp :: Tree -> FilePath -> FilePath -> IO ()
cp (Leaf name) src dst = do
    putStrLn $ "CP " ++ (src </> name) ++ " -> " ++ (dst </> name)
    copyFile (src </> name) (dst </> name)
cp (Node name children) src dst = do
    createDirectoryIfMissing False $ dst </> name
    forM_ children $ \x -> cp x (src </> name) (dst </> name)

rm :: Tree -> FilePath -> IO ()
rm (Leaf name) path = do
    putStrLn $ "RM " ++ (path </> name)
    removeFile $ path </> name
rm (Node name children) path = do
    forM_ children $ \x -> rm x $ path </> name

    empty <- ((==0) . length) <$> listDirectory (path </> name)
    when empty $ do
        putStrLn $ "EMPTY " ++ (path </> name) ++ " --> Removing"
        removeDirectory $ path </> name

register :: FilePath -> IO ()
register lock = do
    fileExist lock >>= \exists -> when exists packageExists

    trees <- fmap mconcat $ forM subdirs $ \x -> do
        exists <- fileExist x 
        if exists then pure <$> build x
        else pure []
    encodeFile lock trees
    putStrLn "Files tree built"

    cp (Node "" trees) "." outdir

remove :: FilePath -> IO ()
remove lock = do
    fileExist lock >>= \exists -> unless exists packageNotFound

    trees <- decodeFile lock :: IO [Tree]
    removeFile lock

    rm (Node "" trees) outdir

usage :: IO ()
usage = do
    putStrLn "Usage: hpm <command> <package_name>"
    putStrLn "Available commands:"
    putStrLn "    reg        Register new package"
    putStrLn "    rem        Remove existing package"
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
    else do
        let lock = config </> last args
        case head args of
            "reg" -> register lock
            "rem" -> remove lock
            _     -> usage
