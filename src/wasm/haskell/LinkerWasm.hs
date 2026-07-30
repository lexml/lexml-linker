{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.UTF8 as StrictUTF8
import Data.List (isPrefixOf)
import GHC.Wasm.Prim (JSString (..), fromJSString, toJSString)
import LexML.Linker
import LexML.URN.Parser (parseURNLexML)

data LinkRequest = LinkRequest
  { requestInput :: String
  , requestInputType :: String
  , requestOutputType :: String
  , requestContext :: String
  , requestResolverUrl :: Maybe String
  , requestBaseUrl :: Maybe String
  , requestTokenLimit :: Maybe Int
  , requestLogRules :: Bool
  , requestLogTokens :: Bool
  }

instance FromJSON LinkRequest where
  parseJSON = withObject "LinkRequest" $ \obj ->
    LinkRequest
      <$> obj .: "input"
      <*> obj .:? "inputType" .!= "text"
      <*> obj .:? "outputType" .!= "urns"
      <*> obj .:? "context" .!= "federal"
      <*> obj .:? "resolverUrl"
      <*> obj .:? "baseUrl"
      <*> obj .:? "tokenLimit"
      <*> obj .:? "logRules" .!= False
      <*> obj .:? "logTokens" .!= False

foreign export javascript "lexml_link"
  lexmlLink :: JSString -> IO JSString

lexmlLink :: JSString -> IO JSString
lexmlLink = fmap (toJSString . LazyUTF8.toString . encode) . linkRequest . fromJSString

linkRequest :: String -> IO Value
linkRequest source =
  case eitherDecodeStrict' (StrictUTF8.fromString source) of
    Left err -> pure $ errorResponse ("Invalid request JSON: " ++ err)
    Right request -> runRequest request

runRequest :: LinkRequest -> IO Value
runRequest request = do
  contextResult <- parseContext (requestContext request)
  case (parseInputType (requestInputType request), parseOutputType (requestOutputType request), contextResult) of
    (Left err, _, _) -> pure $ errorResponse err
    (_, Left err, _) -> pure $ errorResponse err
    (_, _, Left err) -> pure $ errorResponse err
    (Right inputType, Right outputType, Right context) -> do
      let options =
            defaultLinkerOptions
              { loContext = context
              , loBaseUrl = requestBaseUrl request
              , loTokenLimit = requestTokenLimit request
              , loResolverUrl = requestResolverUrl request
              , loInputType = inputType
              , loOutputType = outputType
              , loDebugRegras = requestLogRules request
              , loDebugTokens = requestLogTokens request
              , loConstituicaoSimples = refContextoFederal context
              }
      result <- runExceptT $ linker options (requestInput request)
      pure $ case result of
        Left err -> errorResponse (show err)
        Right output ->
          object
            [ "ok" .= True
            , "content" .= resContent output
            , "partial" .= resPartial output
            ]

parseInputType :: String -> Either String InputType
parseInputType "text" = Right IT_TEXT
parseInputType "hxml" = Right IT_TAGGED
parseInputType value = Left $ "Unknown inputType: " ++ value

parseOutputType :: String -> Either String OutputType
parseOutputType "urns" = Right OT_URNS
parseOutputType "html" = Right OT_AHREF
parseOutputType "xml" = Right OT_XLINK
parseOutputType value = Left $ "Unknown outputType: " ++ value

parseContext :: String -> IO (Either String RefContexto)
parseContext "federal" = pure $ Right $ RC_Nome "federal"
parseContext "senado" = pure $ Right $ RC_Nome "senado"
parseContext value
  | "urn:lex:" `isPrefixOf` value = do
      parsed <- parseURNLexML value
      pure $ maybe (Left $ "Invalid LexML URN context: " ++ value) (Right . RC_URNLexML) parsed
  | otherwise = pure $ Left $ "Unknown context: " ++ value

errorResponse :: String -> Value
errorResponse message =
  object
    [ "ok" .= False
    , "error" .= message
    ]

main :: IO ()
main = pure ()
