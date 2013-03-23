module Main where

import Happstack.Server

main = simpleHTTP nullConf $ ok "Hello, world"
