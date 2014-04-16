{-# LANGUAGE NoMonomorphismRestriction #-}
module Testing (module Test.QuickCheck, module Testing) where

import Test.QuickCheck
import Test.DocTest.Prop

prop2 = propWith (quietArgs{maxSuccess=1000,maxSize=200})
