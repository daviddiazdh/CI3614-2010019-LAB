import System.IO (hFlush, stdout)

data Command = Command {
    instruction :: String,
    parameter :: String
} deriving (Show)

parseCommand :: String -> Maybe Command
parseCommand s = case splitCommand (==' ') s of
    [] -> Nothing
    (x:xs) -> case x of
        "tomar" -> Just Command { instruction = "tomar", parameter = ""}
        "ir" -> Just Command { instruction = "ir", parameter = ""}
        "salir" -> Just Command { instruction = "salir", parameter = ""}
        _ -> Nothing


main :: IO ()
main = gameloop

splitCommand :: (Char -> Bool) -> String -> [String]
splitCommand p "" = [""]
splitCommand p s = case dropWhile p s of 
    "" -> []
    s' -> w : splitCommand p s'' 
        where (w, s'') = break p s'

gameloop :: IO()
gameloop = do
    putStrLn "Ingrese un comando"
    putStr "> "
    hFlush stdout
    command <- getLine
    let parsedCommand = parseCommand(command)
    case parsedCommand of
        Nothing -> putStrLn "Comando inválido."
        Just cmd -> putStrLn (show (instruction cmd))
    let entryList = splitCommand (==' ') command
    let firstCommand = head entryList
    putStrLn ("Envió como primer comando a " ++ firstCommand) 