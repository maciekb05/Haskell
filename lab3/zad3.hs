reverse' :: String -> String
reverse' = foldl (\acc x -> x:acc) []
