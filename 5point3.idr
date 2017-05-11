import Data.Vect

listStringToString : List String -> String
listStringToString xs = foldr1 (\x, acc => x ++ "\n" ++ acc) xs

readToBlank : IO (List String)
readToBlank = do l <- getLine
                 case l of
                    "" => pure []
                    _ => do ls <- readToBlank
                            pure $ l::ls

readAndSave : IO ()
readAndSave = do putStrLn "Enter strings to write: "
                 ls <- readToBlank
                 let s = listStringToString ls
                 putStr "Enter filename: "
                 fn <- getLine
                 Right () <- writeFile fn s
                    | Left err => putStrLn "Error"
                 putStrLn "Written successfully."

readVect : IO (len ** Vect len String)
readVect = do x <- getLine
              if (x == "")
                then pure (_ ** [])
                else do (_ ** xs) <- readVect
                        pure (_ ** x::xs)


-- still grabs newline character
readLinesUntilEmpty : (h : File) -> IO (n ** Vect n String)
readLinesUntilEmpty h = do over <- fEOF h
                           case over of
                             True => pure (_ ** [])
                             False => do Right l <- fGetLine h
                                            | Left err => pure (_ ** [])
                                         (_ ** xs) <- readLinesUntilEmpty h
                                         pure (_ ** l::xs)

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right h <- openFile filename Read
                              | Left err => pure (_ ** [])
                           readLinesUntilEmpty h
