{-# LANGUAGE OverloadedStrings #-}

module Util.Spec
  ( wrongMethodNotAllowed
  , nonJSONUnsupportedMediaType
  , wrongJSONInvalidRequest
  ) where

import Test.Hspec
  ( it
  )
import Test.Hspec.Wai
  ( post
  , request
  , shouldRespondWith
  )

import Util.WAI
  ( postJSON
  )

wrongMethodNotAllowed method path =
  it "responds to an unsupported method with 405 (Method Not Allowed)" $
  request method path [] "" `shouldRespondWith` 405

nonJSONUnsupportedMediaType path =
  it "responds to non-JSON Content-Type with 415 (Unsupported Media Type)" $
  post path "xxx" `shouldRespondWith` 415

wrongJSONInvalidRequest path json =
  it "responds to JSON body representing the wrong data with 400 (Invalid Request)" $
  postJSON path json `shouldRespondWith` 400
