{-# OPTIONS_GHC -Wno-deprecations #-}

module Games.CosmicExpress.Debug (debugWith, debugLn, debugPrefix) where

import Relude

-- Enable debug tracing.
debug :: Bool
debug = False

debugWith :: (a -> String) -> a -> a
debugWith f x = if debug then trace (f x) x else x

debugLn :: String -> a -> a
debugLn s = if debug then trace (s ++ "\n") else id

debugPrefix :: (Show a) => String -> a -> a
debugPrefix prefix = debugWith $ \x -> prefix ++ ": " ++ show x ++ "\n"
