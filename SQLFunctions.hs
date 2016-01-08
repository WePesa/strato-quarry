module SQLFunctions where

clearTrigger :: String -> String -> String
clearTrigger name table = "drop trigger if exists " ++ name ++ " on " ++ table

createTriggerFunction :: String -> String -> String
createTriggerFunction func notify =
  "create or replace function " ++ func ++ "() " ++
  "returns trigger language plpgsql as " ++
  "$$begin \n" ++
  " perform pg_notify('" ++ notify ++ "', null);\n" ++
  " return null;\n" ++
  "end$$"

createTrigger :: String -> String -> String -> String -> String
createTrigger name event table func =
  "create trigger " ++ name ++
  " after " ++ event ++ " on " ++ table ++
  " for each statement execute procedure " ++ func ++ "()"  

listenTrigger :: String -> String
listenTrigger name = "listen " ++ show name
