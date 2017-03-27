import Data.List
import System.IO
import System.Environment

import Path
import qualified DBManip as DM

cmdAdd = ("-a", "--add")
cmdFetch = ("-f", "--fetch-username")
cmdDelete = ("-d", "--delete")
cmdShowDB = ("-db", "--show-database")

cmdPath = ("-p", "--print-database-path")
cmdChangePath = ("-cp", "--change-path")
cmdHelp = ("-h", "--help")

checkCmdTuple :: (String) -> String  -> Bool
checkCmdTuple cmdTuple cmd | cmd == (fst $ cmdTuple) || cmd == (snd $ cmdTuple) = True
                           | otherwise = False

checkCmd :: String -> String 
checkCmd cmdOp  | checkCmdTuple cmdAdd cmdOp = "add"
                | checkCmdTuple cmdFetch cmdOp = "fetch-username"              
                | checkCmdTuple cmdDelete cmdOp = "delete"

                | checkCmdTuple cmdShowDB cmdOp = "show-database"
                | checkCmdTuple cmdPath cmdOp = "print-database-path"
                | checkCmdTuple cmdChangePath cmdOp = "change-path"
                | checkCmdTuple cmdHelp cmdOp = "help"
                | otherwise = "false-arg"


main = do 
    argv <- getArgs
    
    fileH <- openFile ( path ++ fileName ) ReadMode
    contents <- hGetContents fileH
    hClose fileH
    
    case checkCmd $ argv !! 0 of "add" -> DM.appendDB fileName (DM.makeUPTuple (argv !! 1)  (argv !! 2 ++ "\n"))
                                 "fetch-username" -> (lines contents) !! 
    






