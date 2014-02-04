{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

import Language.C.Inline.ObjC
import Language.C.Quote.ObjC


objc_import ["<Foundation/Foundation.h>", "HsFFI.h"]

type Strings = [String]

{-
nslog :: [String] -> IO ()
nslog msg = $(objc ['msg] ''() [cexp| NSLog(@"Here is an array from Haskell: %@", [msg description]) |])
-}
newMsg :: [String] -> IO [String]
newMsg msg = $(objc' ['msg] [t| [String] |] [cexp|
                                       ({  typename NSMutableArray * arr;
                                           arr = [NSMutableArray arrayWithArray: msg];
                                           [arr addObject: @"This is greet from ObjC"];
                                           [NSArray arrayWithArray: arr];
                                        }) |] )

objc_emit


main = do
  objc_initialise
  print =<< newMsg ["I like Objective-C!", "And Haskell!"]
