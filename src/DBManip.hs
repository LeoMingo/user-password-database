module DBManip
(appendDB
) where

import Path

import Data.List
import System.IO


{-the database queries will be named as username-password tuples, 
 - abbreviated as upTuples in this program-}

{-This will be the database format 
 -un1|pw1
 -un2|pw2
 -...
 - -}


appendDB fileName upTuple = do
    fileH <- openFile fileName AppendMode
    hPutStr fileH . fst $ upTuple
    hPutStr fileH "|" 
    hPutStr fileH . snd $ upTuple
    hClose fileH

makeUPTuple :: String -> String -> (String, String)
makeUPTuple unStr pwStr = (unStr, pwStr)


a = "abc|123"

--Remeber the format is username|password
{-
unfoldMaybe :: Maybe t -> t
unfoldMaybe (Just t) = j

pipeLookUp :: Num t => String -> t
pipeLookUp strLine =  unfoldMaybe (elemIndex '|' strLine)

-}

getUsername :: String -> String
getUsername = takeWhile (/= '|') 

usernameList :: [String] -> [String]
usernameList = map getUsername 

getPassword :: String -> String
getPassword = dropWhile (== '|') . dropWhile (/= '|') 

passwordList :: [String] -> [String]
passwordList = map getPassword 

fetchPw :: [String] -> [String] -> String -> String
fetchPw unList pwList un | (elem un unList) = ( pwList !! ((elemIndices un unList) !! 0) )
                         | otherwise = "\"" ++ un  ++ "\" does not exist"












