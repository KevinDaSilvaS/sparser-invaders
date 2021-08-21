module CLI.HelpScreen where

import OutputFormatter.FormatMessage ( _info, _backgroundedInfo )

helpTab :: IO ()
helpTab = do 
    putStrLn $ _backgroundedInfo title 
    putStrLn $ _backgroundedInfo "[parse] " ++ _info parse
    putStrLn $ _backgroundedInfo "[--version | -v] " ++ _info version
    putStrLn $ _backgroundedInfo "[--help | -h] " ++ _info help
    where
        title = "HELP "
        parse = 
            "| parse | .json file path | format " ++ formats ++ " |"
        formats = "(yml, js, *)"
        version = "shows the current version" 
        help = "shows current info" 