module Transpiler.GenerateYmlCode where

genYmlFile ir path = do 
        let lb = formatYml ir ""
        writeFile (path++".yml") lb
    
formatYml (x:xs) spacing
    | x == "[" = "\n" ++ formatArrayYml xs (spacing++"  ")
    | x == "]" = formatYml xs (drop 2 spacing)
    | x == "{" = "\n" ++ formatYml xs (spacing++"  ")
    | x == "}" = formatYml xs (drop 2 spacing)
    | last x == ':' = spacing ++ x ++ " " ++ formatYml xs spacing
    | x == "," = "\n" ++ formatYml xs spacing
    | otherwise = x ++ formatYml xs spacing
formatYml [] _ = ""

formatArrayYml (x:xs) spacing
    | x == "{" = spacing ++ "- " ++ "key" ++ x ++ ":" 
        ++ "\n" ++ formatObjInArr xs (spacing++"  ")
    | x == "}" = formatArrayYml xs (drop 2 spacing)
    | x == "[" = 
        spacing ++ "- " ++ "key" ++ x ++ ":" 
        ++ "\n" ++ formatArrayYml xs (spacing++"  ")
    | x == "]" && 
        (length xs >= 2 && a == "," && b == "[") = 
        formatArrayYml xs (drop 2 spacing)
    | x == "]" && 
        (length xs >= 2 && a == "," && b == "{") = 
        formatArrayYml xs (drop 2 spacing)
    | x == "]" = formatYml (x:xs) spacing
    | x == "," = formatArrayYml xs spacing
    | x == "," && a == "{" = formatArrayYml xs spacing
    | otherwise = spacing ++ "- " ++ x ++ "\n" ++ formatArrayYml xs spacing
    where
        [a, b] = take 2 xs
formatArrayYml [] _ = ""

formatObjInArr (x:xs) spacing 
    | x == "{" = "\n" ++ formatObjInArr xs (spacing++"  ")
    | x == "}" = "\n" ++ formatArrayYml xs (drop 2 spacing)
    | last x == ':' = spacing ++ x ++ " " ++ formatObjInArr xs spacing
    | x == "," = "\n" ++ formatObjInArr xs spacing
    | otherwise = spacing ++ x ++ "\n" ++ formatObjInArr xs spacing
formatObjInArr [] _ = ""
