module SQLFunctions where

clearTrigger :: String -> String -> String
clearTrigger name table = "drop trigger if exists " ++ name ++ " on " ++ table

createTriggerFunction :: String -> String -> String -> String
createTriggerFunction func notify payload =
  "create or replace function " ++ func ++ "() " ++
  "returns trigger language plpgsql as " ++
  "$$begin \n" ++
  " perform pg_notify('" ++ notify ++ "', " ++ payload ++ ");\n" ++
  " return null;\n" ++
  "end$$"

createTrigger :: String -> String -> String -> String
createTrigger name table func =
  "create trigger " ++ name ++
  " after insert on " ++ table ++
  " for each statement execute procedure " ++ func ++ "()"  

listenTrigger :: String -> String
listenTrigger name = "listen " ++ name
