{-# #-}

module Test where

import SimpleJSON

main :: IO ()
main = print "test"

data = JObject ("root",[JString ("value","New"),JString ("onclick","CreateNewDoc()"),JObject ("node",[JString ("value","Open"),JString ("onclick","OpenDoc()")])])
