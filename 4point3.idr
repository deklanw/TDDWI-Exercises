module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size store) newitem = MkData _ (addToData store)
  where
    addToData : Vect oldsize String -> Vect (S oldsize) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String | Get Integer | Size | Search String | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = Just $ Search str
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                    (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store
  = let store_items = items store in
      case integerToFin pos (size store) of
            Nothing => Just ("Out of range\n", store)
            Just id => Just (index id (items store) ++ "\n", store)


printListOfStrings : List String -> String
printListOfStrings xs = foldr (\st, acc => st ++ "\n"  ++ acc) "" xs

tupleListToString : List (Nat, String) -> List String
tupleListToString [] = []
tupleListToString ((x,str) :: xs) = ((show x) ++ " " ++  str) :: (tupleListToString xs)


matchingSecond : String -> (Nat, String) -> Bool
matchingSecond check (x, str) = Strings.isInfixOf check str

search : (str : String) -> (store : DataStore) -> String
search str (MkData Z _) = ""
search str (MkData (S k) items)
  = let
      indexedList = Prelude.List.zip (enumFromTo 0 k) (toList items)
      m = Prelude.List.filter (matchingSecond str) indexedList
      in printListOfStrings (tupleListToString m)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input
  = case parse input of
          Nothing => Just ("Invalid command\n", store)
          Just (Add item) =>
            Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
          Just (Get pos) => getEntry pos store
          Just (Size) => Just (show (size store) ++ "\n", store)
          Just (Search str) => Just (search str store, store)
          Just Quit => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
