{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad
import Data.Binary
import GHC.Generics
import System.Directory
import System.FilePath
import System.Environment
import System.Exit

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
          deriving Generic

instance Binary Tree

build :: FilePath -> IO Tree
build path = do
    let filename = takeFileName path
    dir <- doesDirectoryExist path
    if dir then
        listDirectory path >>= mapM (build . (path</>)) >>= pure . Node filename
    else
        pure $ Leaf filename

cp :: Tree -> FilePath -> FilePath -> IO ()
cp (Leaf name) src dst = do
    let srcFile = src </> name
        dstFile = dst </> name

    symlink <- pathIsSymbolicLink srcFile
    if symlink then do
        targetFile <- getSymbolicLinkTarget srcFile
        putStrLn $ "SYMLINK " ++ (dst </> targetFile) ++ " <- " ++ dstFile
        createFileLink targetFile dstFile
    else do
        putStrLn $ "CP " ++ srcFile ++ " -> " ++ dstFile
        copyFile srcFile dstFile
cp (Node name children) src dst = do
    createDirectoryIfMissing False $ dst </> name
    forM_ children $ \x -> cp x (src </> name) (dst </> name)

rm :: Tree -> FilePath -> IO ()
rm (Leaf name) path = do
    let filename = path </> name
    exists <- doesFileExist filename
    when exists $ do
        putStrLn $ "RM " ++ filename
        removeFile filename
rm (Node name children) path = do
    let dirname = path </> name
    forM_ children $ \x -> rm x dirname

    exists <- doesDirectoryExist dirname
    empty <- if exists then (==0) . length <$> listDirectory dirname else pure False
    when empty $ do
        putStrLn $ "EMPTY " ++ dirname ++ " --> Removing"
        removeDirectory dirname

install :: FilePath -> IO ()
install lock = do
    doesFileExist lock >>= \exists -> when exists packageExists

    trees <- fmap concat $ forM subdirs $ \x -> do
        exists <- doesDirectoryExist x 
        if exists then build x >>= \y -> pure [y]
        else pure []
    putStrLn "Files tree built"

    encodeFile lock trees
    cp (Node "" trees) "." outdir

remove :: FilePath -> IO ()
remove lock = do
    doesFileExist lock >>= \exists -> unless exists packageNotFound

    trees <- decodeFile lock :: IO [Tree]

    rm (Node "" trees) outdir
    removeFile lock

copy :: FilePath -> IO ()
copy lock = do
    doesFileExist lock >>= \exists -> unless exists packageNotFound

    trees <- decodeFile lock :: IO [Tree]
    cp (Node "" trees) outdir "."

usage :: IO ()
usage = do
    putStrLn "Usage: hpm <command> <package_name>"
    putStrLn "Available commands:"
    putStrLn "    ins        Install new package by copying files from current directory"
    putStrLn "    rm         Remove existing package"
    putStrLn "    cpy        Copy package contents into current directory"
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
    if length args /= 2 then
        usage
    else do
        let lock = config </> last args
        case head args of
            "ins" -> install lock
            "rm" -> remove lock
            "cpy" -> copy lock
            _     -> usage
