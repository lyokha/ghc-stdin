GHC frontend plugin GhcStdin
============================

[![Build Status](https://github.com/lyokha/ghc-stdin/workflows/CI/badge.svg)](https://github.com/lyokha/ghc-stdin/actions?query=workflow%3ACI)
[![Hackage](https://img.shields.io/hackage/v/ghc-stdin.svg?label=hackage%20%7C%20ghc-stdin&logo=haskell&logoColor=%239580D1)](https://hackage.haskell.org/package/ghc-stdin)

In GHC, it is not possible to read source code from the standard input.

```ShellSession
$ echo 'module Main where main = putStrLn "Ok"' | ghc -o simple_ok
ghc-9.2.3: no input files
Usage: For basic information, try the `--help' option.
```

This plugin makes this possible.

```ShellSession
$ echo 'module Main where main = putStrLn "Ok"' | ghc --frontend GhcStdin -ffrontend-opt="-o simple_ok"
[1 of 1] Compiling Main             ( ghc-stdin-d8c31cf0ed893d79/ghc-stdin260612-0.hs, ghc-stdin-d8c31cf0ed893d79/ghc-stdin260612-0.o )
Linking simple_ok ...
$ ./simple_ok
Ok
```

Notice that GHC flags are passed via *-ffrontend-opt* in a single string.

Another use case is collecting exported FFI C functions from a module and
putting them in a new shared library.

```ShellSession
$ export NGX_MODULE_PATH=/var/lib/nginx/x86_64-linux-ghc-$(ghc --numeric-version)
$ echo 'module NgxHealthcheck where import NgxExport.Healthcheck ()' | ghc --frontend GhcStdin -ffrontend-opt="-Wall -O2 -dynamic -shared -fPIC -flink-rts -threaded -L$NGX_MODULE_PATH -lngx_healthcheck_plugin -o ngx_healthcheck.so" 
[1 of 1] Compiling NgxHealthcheck   ( ghc-stdin-74de48274545714b/ghc-stdin266454-0.hs, ghc-stdin-74de48274545714b/ghc-stdin266454-0.o )
Linking ngx_healthcheck.so ...
```

(this is a real-world example taken from
[nginx-healthcheck-plugin](https://github.com/lyokha/nginx-healthcheck-plugin)).

Internally, the plugin creates a temporary directory with a temporary source
file inside it with the contents read from the standard input. Then it spawns
another GHC process to compile this file with the options passed in
*-ffrontend-opt*.

#### Installation of the plugin

To use this frontend plugin with GHC, the built module must be registered as a
known GHC package. The simplest way to do it is running

```ShellSession
$ cabal install --lib
```

