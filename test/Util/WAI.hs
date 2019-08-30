{-# LANGUAGE OverloadedStrings #-}

module Util.WAI
  ( postJSON
  ) where

import Data.ByteString.Lazy as LazyBS
import Data.ByteString as BS
import Test.Hspec.Wai
  ( WaiSession
  , request
  )
import Network.HTTP.Types.Method
  ( methodPost
  )
import Network.Wai.Test
  ( SResponse
  )

-- Post some JSON to a path.
-- Return a function from path to a response
postJSON :: BS.ByteString -> (LazyBS.ByteString -> WaiSession SResponse)
postJSON path =
  request methodPost path [("Content-Type", "application/json")]
