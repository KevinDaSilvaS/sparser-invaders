
module CLI.CliOptions where
import Transpiler.TranspilerEntrypoint ( startParsing )
import OutputFormatter.FormatMessage ( _info )
import CLI.HelpScreen ( helpTab )

dispatch :: [([Char], [String] -> IO ())]
dispatch = [
    ("-v", version),
    ("--version", version),
    ("-h", help),
    ("--help", help),
    ("parse", parse)
    ]

version :: p -> IO ()
version _ = do putStrLn $ _info "version 1.0"

help :: p -> IO ()
help _ = do helpTab

parse :: [String] -> IO ()
parse args = do startParsing args