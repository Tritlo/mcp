{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module MCP.Server 
  ( -- * Server Interface
    MCPServer(..)
  , ServerState(..)
  , MCPServerM
  , runMCPServer
  
    -- * Message Handling
  , handleMessage
  , handleRequest
  , handleNotification
  
    -- * Server Runner
  , runServer
  , ServerConfig(..)
  
    -- * Utilities
  , sendResponse
  , sendNotification
  , sendError
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, MonadState, runStateT, get, put, modify)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Data.Aeson (ToJSON, FromJSON, encode, decode, Value, object, toJSON, (.=), fromJSON)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.IO (Handle, stdin, stdout, hFlush)

import MCP.Types
import MCP.Protocol hiding (capabilities)
import qualified MCP.Protocol as Protocol

data ServerState = ServerState
  { serverInitialized :: Bool
  , serverCapabilities :: ServerCapabilities
  , clientCapabilities :: Maybe ClientCapabilities
  , serverInfo :: Maybe Implementation
  , subscriptions :: Map Text ()
  } deriving (Show)

data ServerConfig = ServerConfig
  { configInput :: Handle
  , configOutput :: Handle
  , configServerInfo :: Implementation
  , configCapabilities :: ServerCapabilities
  } deriving (Show)

type MCPServerM = ReaderT ServerConfig (StateT ServerState (ExceptT Text IO))

runMCPServer :: ServerConfig -> ServerState -> MCPServerM a -> IO (Either Text (a, ServerState))
runMCPServer config state action = runExceptT $ runStateT (runReaderT action config) state

initialServerState :: ServerCapabilities -> ServerState
initialServerState caps = ServerState
  { serverInitialized = False
  , serverCapabilities = caps
  , clientCapabilities = Nothing
  , serverInfo = Nothing
  , subscriptions = Map.empty
  }

class Monad m => MCPServer m where
  handleListResources :: ListResourcesParams -> m ListResourcesResult
  handleReadResource :: ReadResourceParams -> m ReadResourceResult
  handleListResourceTemplates :: ListResourceTemplatesParams -> m ListResourceTemplatesResult
  handleListPrompts :: ListPromptsParams -> m ListPromptsResult
  handleGetPrompt :: GetPromptParams -> m GetPromptResult
  handleListTools :: ListToolsParams -> m ListToolsResult
  handleCallTool :: CallToolParams -> m CallToolResult
  handleComplete :: CompleteParams -> m CompleteResult
  handleSetLevel :: SetLevelParams -> m ()

sendResponse :: (MonadIO m, ToJSON a) => Handle -> RequestId -> a -> m ()
sendResponse handle reqId result = liftIO $ do
  let response = JSONRPCResponse "2.0" reqId (toJSON result)
  LBSC.hPutStrLn handle (encode response)
  hFlush handle

sendError :: MonadIO m => Handle -> RequestId -> JSONRPCErrorInfo -> m ()
sendError handle reqId errorInfo = liftIO $ do
  let response = JSONRPCError "2.0" reqId errorInfo
  LBSC.hPutStrLn handle (encode response)
  hFlush handle

sendNotification :: (MonadIO m, ToJSON a) => Handle -> Text -> a -> m ()
sendNotification handle method params = liftIO $ do
  let notification = JSONRPCNotification "2.0" method (Just (toJSON params))
  LBSC.hPutStrLn handle (encode notification)
  hFlush handle

handleMessage :: MCPServer MCPServerM => BSC.ByteString -> MCPServerM (Maybe ())
handleMessage input = do
  case decode (LBS.fromStrict input) :: Maybe JSONRPCMessage of
    Nothing -> do
      config <- ask
      sendError (configOutput config) (RequestId (toJSON ("unknown" :: Text))) $ 
        JSONRPCErrorInfo (-32700) "Parse error" Nothing
      return Nothing
    Just msg -> case msg of
      RequestMessage req -> do
        handleRequest req
        return (Just ())
      NotificationMessage notif -> do
        handleNotification notif
        return (Just ())
      _ -> do
        config <- ask
        sendError (configOutput config) (RequestId (toJSON ("unknown" :: Text))) $ 
          JSONRPCErrorInfo (-32600) "Invalid Request" Nothing
        return Nothing

handleRequest :: MCPServer MCPServerM => JSONRPCRequest -> MCPServerM ()
handleRequest (JSONRPCRequest _ reqId method params) = do
  config <- ask
  state <- get
  
  case method of
    "initialize" -> case params of
      Just p -> case fromJSON p of
        Aeson.Success initParams -> handleInitialize reqId initParams
        Aeson.Error e -> sendError (configOutput config) reqId $ 
                     JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
      Nothing -> sendError (configOutput config) reqId $ 
                   JSONRPCErrorInfo (-32602) "Missing params" Nothing
    
    "ping" -> handlePing reqId
    
    "resources/list" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success listParams -> do
              result <- handleListResources listParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> do
            result <- handleListResources (ListResourcesParams Nothing)
            sendResponse (configOutput config) reqId result
    
    "resources/read" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success readParams -> do
              result <- handleReadResource readParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> sendError (configOutput config) reqId $ 
                       JSONRPCErrorInfo (-32602) "Missing params" Nothing
    
    "resources/templates/list" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success listParams -> do
              result <- handleListResourceTemplates listParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> do
            result <- handleListResourceTemplates (ListResourceTemplatesParams Nothing)
            sendResponse (configOutput config) reqId result
    
    "prompts/list" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success listParams -> do
              result <- handleListPrompts listParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> do
            result <- handleListPrompts (ListPromptsParams Nothing)
            sendResponse (configOutput config) reqId result
    
    "prompts/get" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success getParams -> do
              result <- handleGetPrompt getParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> sendError (configOutput config) reqId $ 
                       JSONRPCErrorInfo (-32602) "Missing params" Nothing
    
    "tools/list" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success listParams -> do
              result <- handleListTools listParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> do
            result <- handleListTools (ListToolsParams Nothing)
            sendResponse (configOutput config) reqId result
    
    "tools/call" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success callParams -> do
              result <- handleCallTool callParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> sendError (configOutput config) reqId $ 
                       JSONRPCErrorInfo (-32602) "Missing params" Nothing
    
    "completion/complete" -> do
      if not (serverInitialized state)
        then sendError (configOutput config) reqId $ 
               JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
        else case params of
          Just p -> case fromJSON p of
            Aeson.Success completeParams -> do
              result <- handleComplete completeParams
              sendResponse (configOutput config) reqId result
            Aeson.Error e -> sendError (configOutput config) reqId $ 
                         JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
          Nothing -> sendError (configOutput config) reqId $ 
                       JSONRPCErrorInfo (-32602) "Missing params" Nothing
    
    "logging/setLevel" -> case params of
      Just p -> case fromJSON p of
        Aeson.Success setLevelParams -> do
          _ <- handleSetLevel setLevelParams
          -- SetLevel response is just an empty object
          sendResponse (configOutput config) reqId (object [])
        Aeson.Error e -> sendError (configOutput config) reqId $ 
                     JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
      Nothing -> sendError (configOutput config) reqId $ 
                   JSONRPCErrorInfo (-32602) "Missing params" Nothing
    
    _ -> sendError (configOutput config) reqId $ 
           JSONRPCErrorInfo (-32601) "Method not found" Nothing

handleInitialize :: RequestId -> InitializeParams -> MCPServerM ()
handleInitialize reqId params = do
  config <- ask
  state <- get
  
  let InitializeParams { capabilities = clientCaps } = params
  
  put state 
    { serverInitialized = True
    , clientCapabilities = Just clientCaps
    , serverInfo = Just (configServerInfo config)
    }
  
  let result = InitializeResult 
        { protocolVersion = "2024-11-05"
        , capabilities = serverCapabilities state
        , serverInfo = configServerInfo config
        , instructions = Nothing
        , _meta = Nothing
        }
  
  sendResponse (configOutput config) reqId result

handlePing :: RequestId -> MCPServerM ()
handlePing reqId = do
  config <- ask
  -- Ping response is just an empty object in MCP
  sendResponse (configOutput config) reqId (object [])

handleNotification :: JSONRPCNotification -> MCPServerM ()
handleNotification notif = do
  return ()

runServer :: MCPServer MCPServerM => ServerConfig -> IO ()
runServer config = do
  let initialState = initialServerState (configCapabilities config)
  
  let loop = do
        line <- liftIO $ BSC.hGetLine (configInput config)
        result <- handleMessage line
        case result of
          Just () -> loop
          Nothing -> return ()
  
  result <- runMCPServer config initialState loop
  case result of
    Left err -> putStrLn $ "Server error: " ++ T.unpack err
    Right _ -> putStrLn "Server terminated"