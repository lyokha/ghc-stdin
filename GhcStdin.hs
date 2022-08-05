-----------------------------------------------------------------------------
-- |
-- Module      :  GhcStdin
-- Copyright   :  (c) Alexey Radkov 2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires GHC with support of plugins)
--
-- A frontend plugin for GHC to compile source code from the standard input.
--
-----------------------------------------------------------------------------


module GhcStdin (frontendPlugin) where

import           GHC.Paths
#if MIN_VERSION_ghc(9,0,2)
import           GHC.Plugins
#else
import           GhcPlugins
#endif
import           Control.Monad
import qualified Data.ByteString as B
import           System.IO
import           System.IO.Temp
import           System.Process
import           System.Exit

-- | Frontend plugin for GHC to compile source code from the standard input.
--
-- In GHC, it is not possible to read source code from the standard input.
--
-- @
-- __$__ echo \'module Main where main = putStrLn \"Ok\"\' | ghc -o simple_ok
-- ghc-9.2.3: no input files
-- Usage: For basic information, try the \`--help\' option.
-- @
--
-- This plugin makes this possible.
--
-- @
-- __$__ echo \'module Main where main = putStrLn \"Ok\"\' | ghc __/--frontend GhcStdin/__ /-ffrontend-opt=\"-o simple_ok\"/
-- [1 of 1] Compiling Main             ( ghc-stdin-d8c31cf0ed893d79\/ghc-stdin260612-0.hs, ghc-stdin-d8c31cf0ed893d79\/ghc-stdin260612-0.o )
-- Linking simple_ok ...
-- __$__ ./simple_ok
-- Ok
-- @
--
-- Notice that GHC flags are passed via /-ffrontend-opt/ in a single string.
--
-- Another use case is collecting exported FFI C functions from a module and
-- putting them in a new shared library.
--
-- @
-- __$__ export NGX_MODULE_PATH=\/var\/lib\/nginx\/x86_64-linux-ghc-9.2.3
-- __$__ echo \'module NgxHealthcheck where import NgxExport.Healthcheck ()\' | ghc __/--frontend GhcStdin/__ /-ffrontend-opt=\"-Wall -O2 -dynamic -shared -fPIC -lHSrts_thr-ghc$(ghc --numeric-version) -L$NGX_MODULE_PATH -lngx_healthcheck_plugin -o ngx_healthcheck.so\"/ 
-- [1 of 1] Compiling NgxHealthcheck   ( ghc-stdin-74de48274545714b\/ghc-stdin266454-0.hs, ghc-stdin-74de48274545714b\/ghc-stdin266454-0.o )
-- Linking ngx_healthcheck.so ...
-- @
--
-- (this is a real-world example taken from
-- [nginx-healthcheck-plugin](https://github.com/lyokha/nginx-healthcheck-plugin)).
--
-- Internally, the plugin creates a temporary directory with a temporary source
-- file inside it with the contents read from the standard input. Then it spawns
-- another GHC process to compile this file with the options passed in
-- /-ffrontend-opt/. Note that the options get collected by 'words' without
-- passing them to a shell preprocessor which means that it is not possible to
-- escape spaces in their values with quotes or backslashes.
frontendPlugin :: FrontendPlugin
frontendPlugin = defaultFrontendPlugin { frontend = compileCodeFromStdin }

compileCodeFromStdin :: FrontendPluginAction
compileCodeFromStdin flags _ = liftIO $
    withTempDirectory "." "ghc-stdin" $ \dir ->
        withTempFile dir "ghc-stdin.hs" $ \src hsrc -> do
            contents <- B.getContents
            B.hPutStr hsrc contents >> hFlush hsrc
            -- FIXME: wordsOfHead won't hide spaces inside quotes correctly,
            -- but using spaces does not seem to be an often case in GHC options
            (_, _, _, h) <- createProcess $ proc ghc $ src : wordsOfHead flags
            r <- waitForProcess h
            unless (r == ExitSuccess) $ exitWith r
    where wordsOfHead [] = []
          wordsOfHead (x : _) = words x

