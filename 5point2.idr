import Data.String
import System

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
                putStr $ "Current guesses: " ++ show (guesses) ++ "\n"
                putStr "Enter guess:\n"
                gs <- getLine
                let g = parsePositive {a=Nat} gs
                let recurse = guess target (guesses + 1)
                case g of
                  Just x => case compare x target of
                              LT => do putStr "Too low.\n"; recurse
                              EQ => putStr "Right!\n"
                              GT => do putStr "Too great!\n"; recurse
                  Nothing => do putStr "Invalid input. Try again.\n"; recurse

main : IO ()
main = do t <- time; guess (fromIntegerNat(1 + (t `mod` 100))) 0


repl' : String -> (String -> String) -> IO ()
repl' str f = do putStr str
                 l <- getLine
                 putStr $ (f l) ++ "\n"
                 repl' str f

replWith' : (state : a) -> (prompt : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
replWith' state prompt onInput = do putStr prompt
                                    inp <- getLine
                                    case onInput state inp of
                                      Just (str, newstate) =>
                                        do putStr $ str
                                           replWith' newstate prompt onInput
                                      Nothing => putStr "Exiting"
