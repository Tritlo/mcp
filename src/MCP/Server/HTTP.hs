{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : MCP.Server.HTTP
-- Description : MCP server implementation for HTTP communication
-- Copyright   : (C) 2025 Matthias Pall Gissurarson
-- License     : MIT
-- Maintainer  : mpg@mpg.is
-- Stability   : experimental
-- Portability : GHC
--
-- This module provides MCP server implementation for HTTP communication.
module MCP.Server.HTTP (
    -- * Server Runner
    runServerHTTP,
    HTTPServerConfig (..),
    -- * Demo Configuration Helpers
    defaultDemoOAuthConfig,
) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar, modifyTVar', readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (encode, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBSC
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Handler, Proxy(..), Server, serve, throwError, serveWithContext, Context(..))
import Servant.API (JSON, Post, ReqBody, (:>), Get, QueryParam, QueryParam', Required, FormUrlEncoded, (:<|>)(..), PlainText)
import Servant.Server (err400, err401, err500, errBody)
import Servant.Auth.Server (Auth, AuthResult(..), JWT, JWTSettings, defaultJWTSettings, generateKey, FromJWT, ToJWT, defaultCookieSettings, makeJWT)
import Crypto.JOSE (JWK)
import GHC.Generics (Generic)

import MCP.Protocol
import MCP.Server (MCPServer(..), MCPServerM, ServerConfig(..), ServerState(..), runMCPServer, initialServerState)
import MCP.Server.Auth (OAuthConfig(..), OAuthProvider(..), validateCodeVerifier, OAuthMetadata(..))
import MCP.Types
import Control.Monad (when, unless)

-- | Configuration for running an MCP HTTP server
data HTTPServerConfig = HTTPServerConfig
    { httpPort :: Port
    , httpBaseUrl :: Text  -- Base URL for OAuth endpoints (e.g., "https://api.example.com")
    , httpServerInfo :: Implementation
    , httpCapabilities :: ServerCapabilities
    , httpEnableLogging :: Bool
    , httpOAuthConfig :: Maybe OAuthConfig
    , httpJWK :: Maybe JWK  -- JWT signing key
    , httpProtocolVersion :: Text  -- MCP protocol version
    }
    deriving (Show)

-- | User information from OAuth
data AuthUser = AuthUser
    { userId :: Text
    , userEmail :: Maybe Text
    , userName :: Maybe Text
    }
    deriving (Show, Generic)

-- | Authorization code with PKCE
data AuthorizationCode = AuthorizationCode
    { authCode :: Text
    , authClientId :: Text
    , authRedirectUri :: Text
    , authCodeChallenge :: Text
    , authCodeChallengeMethod :: Text
    , authScopes :: [Text]
    , authUserId :: Text
    , authExpiry :: UTCTime
    }
    deriving (Show, Generic)

-- | OAuth server state
data OAuthState = OAuthState
    { authCodes :: Map Text AuthorizationCode  -- code -> AuthorizationCode
    , accessTokens :: Map Text AuthUser        -- token -> user
    , refreshTokens :: Map Text (Text, AuthUser) -- refresh_token -> (access_token, user)
    , registeredClients :: Map Text ClientInfo  -- client_id -> ClientInfo
    }
    deriving (Show, Generic)

-- | Client registration request
data ClientRegistrationRequest = ClientRegistrationRequest
    { client_name :: Text
    , redirect_uris :: [Text]
    , grant_types :: [Text]
    , response_types :: [Text]
    , token_endpoint_auth_method :: Text
    }
    deriving (Show, Generic)

instance Aeson.FromJSON ClientRegistrationRequest where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

-- | Client registration response
data ClientRegistrationResponse = ClientRegistrationResponse
    { client_id :: Text
    , client_secret :: Maybe Text
    , client_name :: Text
    , redirect_uris :: [Text]
    , grant_types :: [Text]
    , response_types :: [Text]
    , token_endpoint_auth_method :: Text
    }
    deriving (Show, Generic)

instance Aeson.ToJSON ClientRegistrationResponse where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions

-- | Client info stored in server
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientRedirectUris :: [Text]
    , clientGrantTypes :: [Text]
    , clientResponseTypes :: [Text]
    , clientAuthMethod :: Text
    }
    deriving (Show, Generic)

-- | Token response
data TokenResponse = TokenResponse
    { access_token :: Text
    , token_type :: Text
    , expires_in :: Maybe Int
    , refresh_token :: Maybe Text
    , scope :: Maybe Text
    }
    deriving (Show, Generic)

instance Aeson.ToJSON TokenResponse where
    toJSON TokenResponse{..} = object $
        [ "access_token" .= access_token
        , "token_type" .= token_type
        ] ++ 
        maybe [] (\e -> ["expires_in" .= e]) expires_in ++
        maybe [] (\r -> ["refresh_token" .= r]) refresh_token ++
        maybe [] (\s -> ["scope" .= s]) scope

instance Aeson.FromJSON AuthUser where
    parseJSON = Aeson.withObject "AuthUser" $ \v -> AuthUser
        <$> v Aeson..: "sub"
        <*> v Aeson..:? "email"
        <*> v Aeson..:? "name"

instance Aeson.ToJSON AuthUser where
    toJSON AuthUser{..} = object
        [ "sub" .= userId
        , "email" .= userEmail
        , "name" .= userName
        ]

-- Instances for JWT
instance ToJWT AuthUser
instance FromJWT AuthUser

-- | MCP API definition for HTTP server (following the MCP transport spec)
type MCPAPI auths = Auth auths AuthUser :> "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Unprotected MCP API (for backward compatibility)
type UnprotectedMCPAPI = "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | OAuth endpoints
type OAuthAPI = 
    ".well-known" :> "oauth-authorization-server" :> Get '[JSON] OAuthMetadata
    :<|>
    "register" :> ReqBody '[JSON] ClientRegistrationRequest
               :> Post '[JSON] ClientRegistrationResponse
    :<|>
    "authorize" :> QueryParam' '[Required] "response_type" Text
                :> QueryParam' '[Required] "client_id" Text  
                :> QueryParam' '[Required] "redirect_uri" Text
                :> QueryParam' '[Required] "code_challenge" Text
                :> QueryParam' '[Required] "code_challenge_method" Text
                :> QueryParam "scope" Text
                :> QueryParam "state" Text
                :> Get '[PlainText] Text
    :<|>
    "token" :> ReqBody '[FormUrlEncoded] [(Text, Text)]
            :> Post '[JSON] TokenResponse

-- | Complete API with OAuth
type CompleteAPI auths = OAuthAPI :<|> MCPAPI auths

-- | Create a WAI Application for the MCP HTTP server
mcpApp :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> TVar OAuthState -> JWTSettings -> Application
mcpApp config stateVar oauthStateVar jwtSettings = 
    let cookieSettings = defaultCookieSettings
        authContext = cookieSettings :. jwtSettings :. EmptyContext
        baseApp = case httpOAuthConfig config of
            Just oauthCfg | oauthEnabled oauthCfg -> 
                serveWithContext (Proxy :: Proxy (CompleteAPI '[JWT])) authContext 
                    (oauthServer config oauthStateVar :<|> mcpServerAuth config stateVar)
            _ -> 
                serve (Proxy :: Proxy UnprotectedMCPAPI) (mcpServerNoAuth config stateVar)
    in if httpEnableLogging config
       then logStdoutDev baseApp
       else baseApp
  where
    oauthServer :: HTTPServerConfig -> TVar OAuthState -> Server OAuthAPI
    oauthServer cfg oauthState = 
        handleMetadata cfg :<|> 
        handleRegister cfg oauthState :<|> 
        handleAuthorize cfg oauthState :<|> 
        handleToken jwtSettings cfg oauthState
    
    mcpServerAuth :: HTTPServerConfig -> TVar ServerState -> AuthResult AuthUser -> Aeson.Value -> Handler Aeson.Value
    mcpServerAuth httpConfig stateTVar authResult requestValue = 
        case authResult of
            Authenticated user -> handleHTTPRequest httpConfig stateTVar (Just user) requestValue
            NoSuchUser -> throwError err401 { errBody = encode $ object ["error" .= ("Invalid token" :: Text)] }
            BadPassword -> throwError err401 { errBody = encode $ object ["error" .= ("Invalid token" :: Text)] }
            Indefinite -> throwError err401 { errBody = encode $ object ["error" .= ("Authentication required" :: Text)] }
    
    mcpServerNoAuth :: HTTPServerConfig -> TVar ServerState -> Aeson.Value -> Handler Aeson.Value
    mcpServerNoAuth httpConfig stateTVar = handleHTTPRequest httpConfig stateTVar Nothing

-- | Handle HTTP MCP requests following the MCP transport protocol
handleHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> Maybe AuthUser -> Aeson.Value -> Handler Aeson.Value
handleHTTPRequest httpConfig stateVar _mAuthUser requestValue = do
    -- Parse the incoming JSON-RPC message
    case fromJSON requestValue of
        Aeson.Success (msg :: JSONRPCMessage) -> do
            case msg of
                RequestMessage req -> do
                    -- Process the JSON-RPC request
                    result <- liftIO $ processHTTPRequest httpConfig stateVar req
                    case result of
                        Left err -> throwError err500 { errBody = encode $ object ["error" .= err] }
                        Right response -> return response
                NotificationMessage notif -> do
                    -- Process notifications (no response expected)
                    _ <- liftIO $ processHTTPNotification httpConfig stateVar notif
                    return $ object [] -- Empty response for notifications
                _ -> throwError err400 { errBody = "Invalid JSON-RPC message type" }
        Aeson.Error e -> throwError err400 { errBody = LBSC.pack $ "Invalid JSON-RPC message: " ++ e }

-- | Process an HTTP MCP notification
processHTTPNotification :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> JSONRPCNotification -> IO ()
processHTTPNotification _ _ _ = do
    -- For now, just ignore notifications since they don't need responses
    -- In a more complete implementation, this would handle logging/setLevel notifications
    return ()

-- | Process an HTTP MCP request
processHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> JSONRPCRequest -> IO (Either Text Aeson.Value)
processHTTPRequest httpConfig stateVar req = do
    -- Read the current state
    currentState <- atomically $ readTVar stateVar
    let dummyConfig = ServerConfig
            { configInput = undefined  -- Not used in HTTP mode
            , configOutput = undefined -- Not used in HTTP mode
            , configServerInfo = httpServerInfo httpConfig
            , configCapabilities = httpCapabilities httpConfig
            }
    
    result <- runMCPServer dummyConfig currentState (handleHTTPRequestInner (httpProtocolVersion httpConfig) req)
    case result of
        Left err -> return $ Left err
        Right (response, newState) -> do
            -- Update the state atomically
            atomically $ writeTVar stateVar newState
            return $ Right response

-- | Handle HTTP request within the MCP monad, returning proper JSON-RPC responses
handleHTTPRequestInner :: (MCPServer MCPServerM) => Text -> JSONRPCRequest -> MCPServerM Aeson.Value
handleHTTPRequestInner protocolVersion (JSONRPCRequest _ reqId method params) = do
    config <- ask
    state <- get
    
    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> do
                    handleInitializeHTTP reqId initParams
                    let result = InitializeResult
                            { protocolVersion = protocolVersion
                            , capabilities = configCapabilities config
                            , serverInfo = configServerInfo config
                            , instructions = Nothing
                            , _meta = Nothing
                            }
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "ping" -> return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
        "resources/list" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResources listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResources (ListResourcesParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "resources/read" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success readParams -> do
                            result <- handleReadResource readParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "tools/list" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListTools listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListTools (ListToolsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "tools/call" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success callParams -> do
                            result <- handleCallTool callParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "prompts/list" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListPrompts listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListPrompts (ListPromptsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "prompts/get" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success getParams -> do
                            result <- handleGetPrompt getParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "completion/complete" -> do
            if not (serverInitialized state)
                then return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success completeParams -> do
                            result <- handleComplete completeParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "logging/setLevel" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success setLevelParams -> do
                    _ <- handleSetLevel setLevelParams
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
                Aeson.Error e -> return $ toJSON $ JSONRPCError "2.0" reqId $
                    JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing -> return $ toJSON $ JSONRPCError "2.0" reqId $
                JSONRPCErrorInfo (-32602) "Missing params" Nothing
        _ -> return $ toJSON $ JSONRPCError "2.0" reqId $
            JSONRPCErrorInfo (-32601) "Method not found" Nothing

-- | Handle HTTP initialize request
handleInitializeHTTP :: RequestId -> InitializeParams -> MCPServerM ()
handleInitializeHTTP _ params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps} = params

    put state
        { serverInitialized = True
        , clientCapabilities = Just clientCaps
        , serverInfo = Just (configServerInfo config)
        }

-- | Handle OAuth metadata discovery endpoint
handleMetadata :: HTTPServerConfig -> Handler OAuthMetadata
handleMetadata config = do
    let baseUrl = httpBaseUrl config
        oauthCfg = httpOAuthConfig config
    return OAuthMetadata
        { issuer = baseUrl
        , authorizationEndpoint = baseUrl <> "/authorize"
        , tokenEndpoint = baseUrl <> "/token"
        , registrationEndpoint = Just (baseUrl <> "/register")
        , userInfoEndpoint = Nothing
        , jwksUri = Nothing
        , scopesSupported = fmap supportedScopes oauthCfg
        , responseTypesSupported = maybe ["code"] supportedResponseTypes oauthCfg
        , grantTypesSupported = fmap supportedGrantTypes oauthCfg
        , tokenEndpointAuthMethodsSupported = fmap supportedAuthMethods oauthCfg
        , codeChallengeMethodsSupported = fmap supportedCodeChallengeMethods oauthCfg
        }

-- | Handle dynamic client registration
handleRegister :: HTTPServerConfig -> TVar OAuthState -> ClientRegistrationRequest -> Handler ClientRegistrationResponse
handleRegister config oauthStateVar (ClientRegistrationRequest reqName reqRedirects reqGrants reqResponses reqAuth) = do
    -- Generate client ID
    let prefix = maybe "client_" clientIdPrefix (httpOAuthConfig config)
    clientId <- liftIO $ (prefix <>) <$> generateAuthCode
    
    -- Store client info
    let clientInfo = ClientInfo
            { clientName = reqName
            , clientRedirectUris = reqRedirects
            , clientGrantTypes = reqGrants
            , clientResponseTypes = reqResponses
            , clientAuthMethod = reqAuth
            }
    
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s { registeredClients = Map.insert clientId clientInfo (registeredClients s) }
    
    return ClientRegistrationResponse
        { client_id = clientId
        , client_secret = maybe (Just "") publicClientSecret (httpOAuthConfig config)
        , client_name = reqName
        , redirect_uris = reqRedirects
        , grant_types = reqGrants
        , response_types = reqResponses
        , token_endpoint_auth_method = reqAuth
        }

-- | Handle OAuth authorize endpoint
handleAuthorize :: HTTPServerConfig -> TVar OAuthState -> Text -> Text -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Handler Text
handleAuthorize config oauthStateVar responseType clientId redirectUri codeChallenge codeChallengeMethod mScope mState = do
    -- Validate parameters according to MCP spec
    when (responseType /= "code") $
        throwError err400 { errBody = "Only 'code' response type is supported" }
    
    when (codeChallengeMethod /= "S256") $
        throwError err400 { errBody = "Only 'S256' code challenge method is supported" }
    
    -- Generate authorization code
    code <- liftIO $ generateAuthCodeWithConfig config
    currentTime <- liftIO getCurrentTime
    let expirySeconds = maybe 600 (fromIntegral . authCodeExpirySeconds) (httpOAuthConfig config)
        expiry = addUTCTime expirySeconds currentTime
    
    -- Generate user ID based on configuration
    let oauthCfg = httpOAuthConfig config
        userId = case demoUserIdTemplate =<< oauthCfg of
            Just template -> T.replace "{clientId}" clientId template
            Nothing -> "user-" <> clientId  -- Fallback if no demo mode
        authCode = AuthorizationCode
            { authCode = code
            , authClientId = clientId
            , authRedirectUri = redirectUri
            , authCodeChallenge = codeChallenge
            , authCodeChallengeMethod = codeChallengeMethod
            , authScopes = maybe [] (T.splitOn " ") mScope
            , authUserId = userId
            , authExpiry = expiry
            }
    
    -- Store authorization code
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s { authCodes = Map.insert code authCode (authCodes s) }
    
    -- Return the callback URL with auth code
    let stateParam = maybe "" (\s -> "&state=" <> s) mState
        defaultTemplate = "Authorization successful!\n\n" <>
                         "Redirect to: " <> redirectUri <> "?code=" <> code <> stateParam <> "\n\n" <>
                         "Use this authorization code to exchange for an access token."
        template = maybe defaultTemplate 
                         (\t -> T.replace "{redirectUri}" redirectUri $
                                T.replace "{code}" code $
                                T.replace "{state}" stateParam t)
                         (authorizationSuccessTemplate =<< httpOAuthConfig config)
    return template

-- | Handle OAuth token endpoint
handleToken :: JWTSettings -> HTTPServerConfig -> TVar OAuthState -> [(Text, Text)] -> Handler TokenResponse
handleToken jwtSettings config oauthStateVar params = do
    let paramMap = Map.fromList params
    case Map.lookup "grant_type" paramMap of
        Just "authorization_code" -> handleAuthCodeGrant jwtSettings config oauthStateVar paramMap
        Just "refresh_token" -> handleRefreshTokenGrant jwtSettings config oauthStateVar paramMap
        Just _other -> throwError err400 { errBody = encode $ object ["error" .= ("unsupported_grant_type" :: Text)] }
        Nothing -> throwError err400 { errBody = encode $ object ["error" .= ("invalid_request" :: Text), "error_description" .= ("Missing grant_type" :: Text)] }

-- | Handle authorization code grant  
handleAuthCodeGrant :: JWTSettings -> HTTPServerConfig -> TVar OAuthState -> Map Text Text -> Handler TokenResponse
handleAuthCodeGrant jwtSettings config oauthStateVar params = do
    code <- case Map.lookup "code" params of
        Just c -> return c
        Nothing -> throwError err400 { errBody = encode $ object ["error" .= ("invalid_request" :: Text)] }
    
    codeVerifier <- case Map.lookup "code_verifier" params of
        Just v -> return v
        Nothing -> throwError err400 { errBody = encode $ object ["error" .= ("invalid_request" :: Text)] }
    
    -- Look up authorization code
    oauthState <- liftIO $ readTVarIO oauthStateVar
    authCode <- case Map.lookup code (authCodes oauthState) of
        Just ac -> return ac
        Nothing -> throwError err400 { errBody = encode $ object ["error" .= ("invalid_grant" :: Text)] }
    
    -- Verify code hasn't expired
    currentTime <- liftIO getCurrentTime
    when (currentTime > authExpiry authCode) $
        throwError err400 { errBody = encode $ object ["error" .= ("invalid_grant" :: Text), "error_description" .= ("Authorization code expired" :: Text)] }
    
    -- Verify PKCE
    unless (validateCodeVerifier codeVerifier (authCodeChallenge authCode)) $
        throwError err400 { errBody = encode $ object ["error" .= ("invalid_grant" :: Text), "error_description" .= ("Invalid code verifier" :: Text)] }
    
    -- Create user for JWT
    let oauthCfg = httpOAuthConfig config
        emailDomain = maybe "example.com" demoEmailDomain oauthCfg
        userName = maybe "User" demoUserName oauthCfg
        user = AuthUser
            { userId = authUserId authCode
            , userEmail = Just $ authUserId authCode <> "@" <> emailDomain
            , userName = Just userName
            }
    
    -- Generate JWT access token
    accessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case accessTokenResult of
        Left _err -> throwError err500 { errBody = encode $ object ["error" .= ("Token generation failed" :: Text)] }
        Right accessToken -> do
            -- Generate refresh token
            refreshToken <- liftIO $ generateRefreshTokenWithConfig config
            
            let accessTokenText = TE.decodeUtf8 $ LBS.toStrict accessToken
            
            -- Store tokens
            liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
                s { authCodes = Map.delete code (authCodes s)
                  , accessTokens = Map.insert accessTokenText user (accessTokens s)
                  , refreshTokens = Map.insert refreshToken (accessTokenText, user) (refreshTokens s)
                  }
    
            return TokenResponse
                { access_token = accessTokenText
                , token_type = "Bearer"
                , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
                , refresh_token = Just refreshToken
                , scope = if null (authScopes authCode) then Nothing else Just (T.intercalate " " (authScopes authCode))
                }

-- | Handle refresh token grant
handleRefreshTokenGrant :: JWTSettings -> HTTPServerConfig -> TVar OAuthState -> Map Text Text -> Handler TokenResponse
handleRefreshTokenGrant jwtSettings config oauthStateVar params = do
    refreshToken <- case Map.lookup "refresh_token" params of
        Just t -> return t
        Nothing -> throwError err400 { errBody = encode $ object ["error" .= ("invalid_request" :: Text)] }
    
    -- Look up refresh token
    oauthState <- liftIO $ readTVarIO oauthStateVar
    (oldAccessToken, user) <- case Map.lookup refreshToken (refreshTokens oauthState) of
        Just info -> return info
        Nothing -> throwError err400 { errBody = encode $ object ["error" .= ("invalid_grant" :: Text)] }
    
    -- Generate new JWT access token
    newAccessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case newAccessTokenResult of
        Left _err -> throwError err500 { errBody = encode $ object ["error" .= ("Token generation failed" :: Text)] }
        Right newAccessToken -> do
            let newAccessTokenText = TE.decodeUtf8 $ LBS.toStrict newAccessToken
            
            -- Update tokens
            liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
                s { accessTokens = Map.insert newAccessTokenText user $ Map.delete oldAccessToken (accessTokens s)
                  , refreshTokens = Map.insert refreshToken (newAccessTokenText, user) (refreshTokens s)
                  }
            
            return TokenResponse
                { access_token = newAccessTokenText
                , token_type = "Bearer"
                , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
                , refresh_token = Just refreshToken
                , scope = Nothing
                }

-- | Generate random authorization code
generateAuthCode :: IO Text
generateAuthCode = do
    uuid <- UUID.nextRandom
    return $ "code_" <> UUID.toText uuid

-- | Generate authorization code with configurable prefix
generateAuthCodeWithConfig :: HTTPServerConfig -> IO Text
generateAuthCodeWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "code_" authCodePrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid


-- | Generate refresh token with configurable prefix
generateRefreshTokenWithConfig :: HTTPServerConfig -> IO Text
generateRefreshTokenWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "rt_" refreshTokenPrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid

-- | Default demo OAuth configuration for testing purposes
defaultDemoOAuthConfig :: OAuthConfig
defaultDemoOAuthConfig = OAuthConfig
    { oauthEnabled = True
    , oauthProviders = []
    , tokenValidationEndpoint = Nothing
    , requireHTTPS = False  -- For demo only
    -- Default timing parameters
    , authCodeExpirySeconds = 600  -- 10 minutes
    , accessTokenExpirySeconds = 3600  -- 1 hour
    -- Default OAuth parameters
    , supportedScopes = ["mcp:read", "mcp:write"]
    , supportedResponseTypes = ["code"]
    , supportedGrantTypes = ["authorization_code", "refresh_token"]
    , supportedAuthMethods = ["none"]
    , supportedCodeChallengeMethods = ["S256"]
    -- Demo mode settings
    , autoApproveAuth = True
    , demoUserIdTemplate = Just "test-user-{clientId}"
    , demoEmailDomain = "example.com"
    , demoUserName = "Test User"
    , publicClientSecret = Just ""
    -- Default token prefixes
    , authCodePrefix = "code_"
    , refreshTokenPrefix = "rt_"
    , clientIdPrefix = "client_"
    -- Default response template
    , authorizationSuccessTemplate = Nothing
    }

-- | Run the MCP server as an HTTP server
runServerHTTP :: (MCPServer MCPServerM) => HTTPServerConfig -> IO ()
runServerHTTP config = do
    -- Initialize JWT settings if OAuth is enabled
    jwtSettings <- case httpJWK config of
        Just jwk -> return $ defaultJWTSettings jwk
        Nothing -> do
            key <- generateKey
            return $ defaultJWTSettings key
    
    -- Initialize the server state
    stateVar <- newTVarIO $ initialServerState (httpCapabilities config)
    
    -- Initialize OAuth state
    oauthStateVar <- newTVarIO $ OAuthState
        { authCodes = Map.empty
        , accessTokens = Map.empty
        , refreshTokens = Map.empty
        , registeredClients = Map.empty
        }
    
    putStrLn $ "Starting MCP HTTP Server on port " ++ show (httpPort config) ++ "..."
    
    when (maybe False oauthEnabled (httpOAuthConfig config)) $ do
        putStrLn "OAuth authentication enabled"
        putStrLn $ "Authorization endpoint: " ++ T.unpack (httpBaseUrl config) ++ "/authorize"
        putStrLn $ "Token endpoint: " ++ T.unpack (httpBaseUrl config) ++ "/token"
        case httpOAuthConfig config >>= \cfg -> if null (oauthProviders cfg) then Nothing else Just (oauthProviders cfg) of
            Just providers -> do
                putStrLn $ "OAuth providers: " ++ T.unpack (T.intercalate ", " (map providerName providers))
                when (any requiresPKCE providers) $ putStrLn "PKCE enabled (required by MCP spec)"
            Nothing -> return ()
    
    run (httpPort config) (mcpApp config stateVar oauthStateVar jwtSettings)
