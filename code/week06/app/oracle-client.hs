{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson
import Data.Aeson.Types       (parseMaybe)
import Data.ByteString        (ByteString)
import Data.ByteString.Char8  (unpack)
import Data.Maybe             (fromJust)
import Data.Proxy             (Proxy (..))
import Data.Text              (pack, Text)
import Data.UUID
import Network.HTTP.Req
import Text.Regex.TDFA

main :: IO ()
main = do
    uuid <- read <$> readFile "oracle.cid"
    putStrLn $ "oracle contract instance id: " ++ show uuid
    go uuid Nothing
  where
    go :: UUID -> Maybe Integer -> IO a
    go uuid m = do
        x <- getExchangeRate
        let y = Just x
        when (m /= y) $
            updateOracle uuid x
        threadDelay 5_000_000
        go uuid y

updateOracle :: UUID -> Integer -> IO ()
updateOracle uuid x = runReq defaultHttpConfig $ do
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "update")
        (ReqBodyJson x)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "updated oracle to " ++ show x
        else "error updating oracle"

getExchangeRate :: IO Integer
getExchangeRate = runReq defaultHttpConfig $ do
    v <- req
        GET
        (https "coinmarketcap.com" /: "currencies" /: "cardano")
        NoReqBody
        bsResponse
        mempty
    let priceRegex      = "priceValue___11gHJ\">\\$([\\.0-9]*)" :: ByteString
        (_, _, _, [bs]) = responseBody v =~ priceRegex :: (ByteString, ByteString, ByteString, [ByteString])
        d               = read $ unpack bs :: Double
        x               = round $ 1_000_000 * d
    liftIO $ putStrLn $ "queried exchange rate: " ++ show d
    return x

data Payload = Payload
  { id :: String
  , vs_currency :: String
  , days :: Integer
  }

getExchangeRateCoingecko :: IO Integer
getExchangeRateCoingecko = runReq defaultHttpConfig $ do
    v :: Object <- responseBody <$> req
        GET
        (https "api.coingecko.com" /: "api" /: "v3" /: "simple" /: "price")
        NoReqBody
        jsonResponse
        ("ids" =: ("cardano" :: Text) <> "vs_currencies" =: ("usd" :: Text))

    let
      exch_rate :: Double
      exch_rate = fromJust $ flip parseMaybe v $ \obj -> do
            crd_vals :: Object <- obj .: "cardano"
            usd_obj :: Value <- crd_vals .: "usd"
            parseJSON usd_obj
      rate = round $ exch_rate * 1_000_000
    liftIO $ putStrLn $ "queried exchange rate: " ++ show exch_rate
    return rate
