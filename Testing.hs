{-# LANGUAGE NoMonomorphismRestriction #-}
module Testing (module Test.QuickCheck, module Testing) where

import Test.QuickCheck

-- | @prop@ with customized arguments.
propWith :: Testable p => Args -> p -> IO ()
propWith args p = do
  r <- quickCheckWithResult args  p
  case r of
    Success _ _ _ -> return ()
    _ -> putStrLn $ output r

prop = propWith (stdArgs{maxSuccess=1000,maxSize=200,chatty=False})
