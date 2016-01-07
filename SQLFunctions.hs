module SQLFunctions where

clearTrigger :: String -> String -> String
clearTrigger name table = "drop trigger if exists " ++ name ++ " on " ++ table

createTriggerFunction :: String -> String -> String -> String
createTriggerFunction func notify payload =
  "create or replace function " ++ func ++ "() " ++
  "returns trigger language plpgsql as " ++
  "$$begin " ++
  " perform pg_notify('" ++ notify ++ "', " ++ payload ++ ")" ++
  " return null; " ++
  "end$$"

createTrigger :: String -> String -> String -> String -> String -> String -> String
createTrigger name event table each cond func =
  "create trigger " ++ name ++
  " after insert on " ++ table ++
  " for each " ++ each ++
  (if not . null $ cond then "when (" ++ cond ++ ")" else "") ++
  " execute procedure " ++ func ++ "()"  

listenTrigger :: String -> String
listenTrigger name = "listen " ++ name
