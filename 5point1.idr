printLength : IO ()
printLength = do
                  putStr "Input string: "
                  input <- getLine
                  let len = length input
                  putStrLn (show len)


printLonger : IO ()
printLonger = do
                  putStr "Enter two strings:\n"
                  input1 <- getLine
                  input2 <- getLine
                  let greater = if length input1 >= length input2 then input1 else input2
                  putStr $ show (length greater) ++ "\n"

printLonger' : IO ()
printLonger' = putStr "Enter two strings:\n" >>= (\_ => getLine >>=
    (\input1 => getLine >>= (\input2 => let greater = if length input1 >= length input2 then input1 else input2 in putStr $ show (length greater) ++ "\n")))

printLonger'' : IO ()
printLonger'' = putStr "Enter two strings:\n" *> getLine >>=
    (\input1 => getLine >>= (\input2 => let greater = if length input1 >= length input2 then input1 else input2 in putStr $ show (length greater) ++ "\n"))
