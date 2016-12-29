module Lib where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Data.Time.Clock
import           Network.HTTP

remoteSquare :: Integer -> IO Integer
remoteSquare n = do
    result <- simpleHTTP (getRequest $ "http://127.0.0.1:9090/square?num=" `mappend` show n)
    rsp <- getResponseBody result
    return $ read rsp

remoteSquareSum0 :: Integer -> Integer -> IO Integer
remoteSquareSum0 x y = do
    sx <- remoteSquare x
    sy <- remoteSquare y
    return $ sx + sy

remoteSquareSum1 :: (Traversable t) => t Integer -> IO Integer
remoteSquareSum1 nums = sum <$> mapM remoteSquare nums

type MyAsync = MVar

myAsync :: IO a -> IO (MyAsync a)
myAsync action = do
    as <- newEmptyMVar
    forkIO $ do
        a <- action
        putMVar as a
    return as

myWait :: MyAsync a -> IO a
myWait = readMVar

remoteSquareSumMyAsync :: Integer -> Integer -> IO Integer
remoteSquareSumMyAsync x y = do
    ax <- myAsync $ remoteSquare x
    ay <- myAsync $ remoteSquare y
    sx <- myWait ax
    sy <- myWait ay
    return $ sx + sy

remoteSquareSumAsync0 :: Integer -> Integer -> IO Integer
remoteSquareSumAsync0 x y = do
    ax <- async $ remoteSquare x
    ay <- async $ remoteSquare y
    sx <- wait ax
    sy <- wait ay
    return $ sx + sy

remoteSquareSumAsync1 :: Integer -> Integer -> IO Integer
remoteSquareSumAsync1 x y =
    withAsync (remoteSquare x) $ \ax ->
    withAsync (remoteSquare y) $ \ay -> do
        (sx, sy) <- waitBoth ax ay
        return $ sx + sy

remoteSquareSumAsync2 :: Integer -> Integer -> IO Integer
remoteSquareSumAsync2 x y = do
    (sx, sy) <- concurrently (remoteSquare x) (remoteSquare y)
    return $ sx + sy

remoteSquareSumAsync3 :: Integer -> Integer -> IO Integer
remoteSquareSumAsync3 x y = do
    ss <- mapConcurrently remoteSquare [x, y]
    return $ sum ss

remoteSquareSumAsync4 :: (Traversable t) => t Integer -> IO Integer
remoteSquareSumAsync4 nums = sum <$> mapConcurrently remoteSquare nums

timeit :: IO a -> IO (a, NominalDiffTime)
timeit action = do
    start <- getCurrentTime
    a <- action
    end <- getCurrentTime
    return (a, diffUTCTime end start)
