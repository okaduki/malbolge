{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( exec
    ) where

import Data.Char (isSpace, ord, chr)
import Data.Array.IO
import System.IO.Error
import Control.Exception

type State = (Int,Int,Int,IOArray Int Int)

sh2 :: Int
sh2 = 65536

xlat1 :: Int -> Char
xlat1 i =
  "+b(29e*j1VMEKLyC})8&m#~W>qxdRp0wkrUo[D7,XTcA\"lI\
  \.v%{gJh4G\\-=O@5`_3i<?Z';FNQuY]szf$!BS/|t:Pn6^Ha" !! i

xlat2 :: Int -> Char
xlat2 i =
  "5z]&gqtyfr$(we4{WP)H-Zn,[%\\3dL+Q;>U!pJS72FhOA1C\
  \B6v^=I_0/8|jsb9m<.TVac`uY*MK'X~xDl}REokN:#?G\"i@" !! i

op :: Int -> Int -> Int
op x y = foldl (\i p -> (i + (o9 !! (y `div` p `mod` 9) !! (x `div` p `mod` 9)) * p) `mod` sh2) 0 p9
  where
    p9 = [1, 9, 81, 729, 6561]
    o9 = [
      [ 4, 3, 3, 1, 0, 0, 1, 0, 0 ],
      [ 4, 3, 5, 1, 0, 2, 1, 0, 2 ],
      [ 5, 5, 4, 2, 2, 1, 2, 2, 1 ],
      [ 4, 3, 3, 1, 0, 0, 7, 6, 6 ],
      [ 4, 3, 5, 1, 0, 2, 7, 6, 8 ],
      [ 5, 5, 4, 2, 2, 1, 8, 8, 7 ],
      [ 7, 6, 6, 7, 6, 6, 4, 3, 3 ],
      [ 7, 6, 8, 7, 6, 8, 4, 3, 5 ],
      [ 8, 8, 7, 8, 8, 7, 5, 5, 4 ]
      ]

getChar0 :: IO Char
getChar0 = handle handleEOF getChar
  where handleEOF e = if isEOFError e then return '\0' else throwIO e

exec1 :: State -> IO State
exec1 (a,c,d,mem) = readArray mem c >>= \ch -> case () of
  _
    | (ch < 33 || ch > 126) -> exec1(a,c,d,mem)
    | (xlat1 $ (ch - 33 + c) `mod` 94) == 'v' -> return (a,c,d,mem)
    | otherwise -> do
        (a2,c2,d2) <- upd $ xlat1 $ (ch - 33 + c) `mod` 94
        ch2 <- readArray mem c2
        writeArray mem c2 $ ord $ xlat2 (ch2 - 33)
        exec1 (a2,
               (c2+1) `mod` 59049,
               (d2+1) `mod` 59049,
               mem)
  where
    upd 'j' = readArray mem d >>= \d' -> return (a,c,d')
    upd 'i' = readArray mem d >>= \c' -> return (a,c',d)
    upd '*' = do
      x <- readArray mem d
      let x' = (x `div` 3 + x `mod` 3 * 19683) `mod` sh2
      writeArray mem d x'
      return (x',c,d)
    upd 'p' = do
      x <- readArray mem d
      let x' = op a x
      writeArray mem d x'
      return (x',c,d)
    upd '<' = putChar (chr $ a `mod` 256) >> return (a,c,d)
    upd '/' = do
      x <- ord <$> getChar0
      return (if x == 0 then 59048 else x,c,d)
    upd  _  = return (a,c,d)

exec :: String -> IO ()
exec prog = do
  mem <- newArray_ (0, 59048) :: IO (IOArray Int Int)
  errmsg <- initMem prog mem
  if errmsg == ""
    then exec1 (0,0,0,mem) >> return ()
    else putStrLn errmsg

initMem :: String -> IOArray Int Int -> IO String
initMem s mem = loop (s,0)
  where
    loop ([],59049) = return ""
    loop ( _,59049) = return "Source code is too long." 
    loop ([],i)     = do
      v1 <- readArray mem (i-1)
      v2 <- readArray mem (i-2)
      writeArray mem i $ op v1 v2
      loop ([],i+1)
    loop (c:cs,i) =
      if isSpace c
      then loop (cs,i)
      else
        if (32 < ord c && ord c < 127)
           && not ((xlat1 $ (ord c - 33 + i) `mod` 94) `elem` "ji*p</vo")
        then return "An invalid character appears."
        else writeArray mem i (ord c) >> loop (cs,i+1)
