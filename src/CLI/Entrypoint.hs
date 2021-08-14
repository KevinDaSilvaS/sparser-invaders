module CLI.Entrypoint where

entrypoint :: Maybe (t -> IO ()) -> t -> IO ()
entrypoint (Just action) args = action args
entrypoint Nothing _ = return ()