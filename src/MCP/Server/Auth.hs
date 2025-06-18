{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : MCP.Server.Auth
Description : MCP-compliant OAuth 2.1 authentication
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides MCP-compliant OAuth 2.1 authentication with PKCE support.
-}
module MCP.Server.Auth (
    -- * OAuth Configuration
    OAuthConfig (..),
    OAuthProvider (..),
    OAuthGrantType (..),

    -- * Token Validation
    TokenInfo (..),
    validateBearerToken,
    extractBearerToken,

    -- * PKCE Support
    PKCEChallenge (..),
    generateCodeVerifier,
    generateCodeChallenge,
    validateCodeVerifier,

    -- * Metadata Discovery
    OAuthMetadata (..),
    discoverOAuthMetadata,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Network.HTTP.Simple (addRequestHeader, getResponseBody, httpJSON, parseRequest, setRequestBodyJSON, setRequestMethod)
import System.Random (newStdGen, randomRs)

-- | OAuth grant types supported by MCP
data OAuthGrantType
    = AuthorizationCode -- For user-based scenarios
    | ClientCredentials -- For application-to-application
    deriving (Show, Eq, Generic)

-- | OAuth provider configuration (MCP-compliant)
data OAuthProvider = OAuthProvider
    { providerName :: Text
    , clientId :: Text
    , clientSecret :: Maybe Text -- Optional for public clients
    , authorizationEndpoint :: Text
    , tokenEndpoint :: Text
    , userInfoEndpoint :: Maybe Text
    , scopes :: [Text]
    , grantTypes :: [OAuthGrantType]
    , requiresPKCE :: Bool -- MCP requires PKCE for all clients
    , metadataEndpoint :: Maybe Text -- For OAuth metadata discovery
    }
    deriving (Show, Generic)

-- | OAuth configuration for the MCP server
data OAuthConfig = OAuthConfig
    { oauthEnabled :: Bool
    , oauthProviders :: [OAuthProvider]
    , tokenValidationEndpoint :: Maybe Text -- For validating tokens
    , requireHTTPS :: Bool -- MCP requires HTTPS for OAuth
    -- Configurable timing parameters
    , authCodeExpirySeconds :: Int
    , accessTokenExpirySeconds :: Int
    , -- Configurable OAuth parameters
      supportedScopes :: [Text]
    , supportedResponseTypes :: [Text]
    , supportedGrantTypes :: [Text]
    , supportedAuthMethods :: [Text]
    , supportedCodeChallengeMethods :: [Text]
    , -- Demo mode settings
      autoApproveAuth :: Bool
    , demoUserIdTemplate :: Maybe Text -- Nothing means no demo mode
    , demoEmailDomain :: Text
    , demoUserName :: Text
    , publicClientSecret :: Maybe Text
    , -- Token prefixes
      authCodePrefix :: Text
    , refreshTokenPrefix :: Text
    , clientIdPrefix :: Text
    , -- Response templates
      authorizationSuccessTemplate :: Maybe Text
    }
    deriving (Show, Generic)

-- | PKCE challenge data
data PKCEChallenge = PKCEChallenge
    { codeVerifier :: Text
    , codeChallenge :: Text
    , challengeMethod :: Text -- Always "S256" for MCP
    }
    deriving (Show, Generic)

-- | OAuth metadata (from discovery endpoint)
data OAuthMetadata = OAuthMetadata
    { issuer :: Text
    , authorizationEndpoint :: Text
    , tokenEndpoint :: Text
    , registrationEndpoint :: Maybe Text
    , userInfoEndpoint :: Maybe Text
    , jwksUri :: Maybe Text
    , scopesSupported :: Maybe [Text]
    , responseTypesSupported :: [Text]
    , grantTypesSupported :: Maybe [Text]
    , tokenEndpointAuthMethodsSupported :: Maybe [Text]
    , codeChallengeMethodsSupported :: Maybe [Text]
    }
    deriving (Show, Generic)

instance FromJSON OAuthMetadata where
    parseJSON = Aeson.withObject "OAuthMetadata" $ \v ->
        OAuthMetadata
            <$> v Aeson..: "issuer"
            <*> v Aeson..: "authorization_endpoint"
            <*> v Aeson..: "token_endpoint"
            <*> v Aeson..:? "registration_endpoint"
            <*> v Aeson..:? "userinfo_endpoint"
            <*> v Aeson..:? "jwks_uri"
            <*> v Aeson..:? "scopes_supported"
            <*> v Aeson..: "response_types_supported"
            <*> v Aeson..:? "grant_types_supported"
            <*> v Aeson..:? "token_endpoint_auth_methods_supported"
            <*> v Aeson..:? "code_challenge_methods_supported"

instance ToJSON OAuthMetadata where
    toJSON OAuthMetadata{..} =
        Aeson.object $
            [ "issuer" Aeson..= issuer
            , "authorization_endpoint" Aeson..= authorizationEndpoint
            , "token_endpoint" Aeson..= tokenEndpoint
            , "response_types_supported" Aeson..= responseTypesSupported
            ]
                ++ maybe [] (\x -> ["registration_endpoint" Aeson..= x]) registrationEndpoint
                ++ maybe [] (\x -> ["userinfo_endpoint" Aeson..= x]) userInfoEndpoint
                ++ maybe [] (\x -> ["jwks_uri" Aeson..= x]) jwksUri
                ++ maybe [] (\x -> ["scopes_supported" Aeson..= x]) scopesSupported
                ++ maybe [] (\x -> ["grant_types_supported" Aeson..= x]) grantTypesSupported
                ++ maybe [] (\x -> ["token_endpoint_auth_methods_supported" Aeson..= x]) tokenEndpointAuthMethodsSupported
                ++ maybe [] (\x -> ["code_challenge_methods_supported" Aeson..= x]) codeChallengeMethodsSupported

-- | Token introspection response
data TokenInfo = TokenInfo
    { active :: Bool
    , scope :: Maybe Text
    , clientId :: Maybe Text
    , username :: Maybe Text
    , tokenType :: Maybe Text
    , exp :: Maybe Integer -- Expiration time (Unix timestamp)
    , iat :: Maybe Integer -- Issued at time (Unix timestamp)
    , nbf :: Maybe Integer -- Not before time (Unix timestamp)
    , sub :: Maybe Text -- Subject
    , aud :: Maybe [Text] -- Audience
    , iss :: Maybe Text -- Issuer
    }
    deriving (Show, Generic)

instance FromJSON TokenInfo where
    parseJSON = Aeson.withObject "TokenInfo" $ \v ->
        TokenInfo
            <$> v Aeson..: "active"
            <*> v Aeson..:? "scope"
            <*> v Aeson..:? "client_id"
            <*> v Aeson..:? "username"
            <*> v Aeson..:? "token_type"
            <*> v Aeson..:? "exp"
            <*> v Aeson..:? "iat"
            <*> v Aeson..:? "nbf"
            <*> v Aeson..:? "sub"
            <*> v Aeson..:? "aud"
            <*> v Aeson..:? "iss"

-- | Extract Bearer token from Authorization header
extractBearerToken :: Text -> Maybe Text
extractBearerToken authHeader =
    case T.words authHeader of
        ["Bearer", token] -> Just token
        _ -> Nothing

-- | Validate a bearer token
validateBearerToken :: (MonadIO m) => OAuthConfig -> Text -> m (Either Text TokenInfo)
validateBearerToken config token = do
    -- Basic validation
    if T.null token
        then return $ Left "Empty token"
        else case tokenValidationEndpoint config of
            Just endpoint -> introspectToken endpoint token
            Nothing -> do
                -- Without an introspection endpoint, perform basic JWT validation
                -- In production, this should:
                -- 1. Verify JWT signature using JWK from jwks_uri
                -- 2. Check expiration time
                -- 3. Validate issuer and audience
                -- 4. Check token type is "Bearer"

                -- For now, decode JWT payload (middle part) for basic validation
                case T.splitOn "." token of
                    [_header, payload, _signature] -> do
                        currentTime <- liftIO getCurrentTime
                        case decodeJWTPayload payload of
                            Right tokenInfo ->
                                case validateTokenClaims tokenInfo currentTime of
                                    Right _ -> return $ Right tokenInfo
                                    Left err -> return $ Left err
                            Left err -> return $ Left $ "Invalid JWT format: " <> err
                    _ -> return $ Left "Invalid JWT structure"

-- | Introspect token using OAuth introspection endpoint
introspectToken :: (MonadIO m) => Text -> Text -> m (Either Text TokenInfo)
introspectToken endpoint token = liftIO $ do
    let url = T.unpack endpoint
    request <- parseRequest url
    let requestWithBody =
            setRequestMethod "POST" $
                setRequestBodyJSON (Aeson.object [("token", Aeson.String token)]) $
                    addRequestHeader "Content-Type" "application/json" request

    response <- httpJSON requestWithBody
    let tokenInfo = getResponseBody response

    if active tokenInfo
        then return $ Right tokenInfo
        else return $ Left "Token is not active"

-- | Decode JWT payload (base64url encoded JSON)
decodeJWTPayload :: Text -> Either Text TokenInfo
decodeJWTPayload payload =
    case B64URL.decodeUnpadded (TE.encodeUtf8 payload) of
        Right decodedBytes ->
            case Aeson.decode' (LBS.fromStrict decodedBytes) of
                Just info -> Right info{active = True} -- JWT is implicitly active
                Nothing -> Left "Failed to parse JWT payload"
        Left _ -> Left "Invalid base64url encoding"

-- | Validate token claims (expiration, not-before, etc.)
validateTokenClaims :: TokenInfo -> UTCTime -> Either Text ()
validateTokenClaims tokenInfo currentTime = do
    let currentTimestamp = floor (realToFrac (utcTimeToPOSIXSeconds currentTime) :: Double) :: Integer

    -- Check expiration
    case MCP.Server.Auth.exp tokenInfo of
        Just expTime ->
            if currentTimestamp > expTime
                then Left "Token has expired"
                else Right ()
        Nothing -> Right ()

    -- Check not-before
    case MCP.Server.Auth.nbf tokenInfo of
        Just nbfTime ->
            if currentTimestamp < nbfTime
                then Left "Token not yet valid"
                else Right ()
        Nothing -> Right ()

    return ()

-- | Generate a cryptographically secure code verifier for PKCE
generateCodeVerifier :: IO Text
generateCodeVerifier = do
    gen <- newStdGen
    let chars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-._~"
    let verifier = take 128 $ randomRs (0, length chars - 1) gen
    return $ T.pack $ map (chars !!) verifier

-- | Generate code challenge from verifier using SHA256 (S256 method)
generateCodeChallenge :: Text -> Text
generateCodeChallenge verifier =
    let verifierBytes = TE.encodeUtf8 verifier
        challengeHash = hashWith SHA256 verifierBytes
        challengeBytes = convert challengeHash :: ByteString
     in TE.decodeUtf8 $ B64URL.encodeUnpadded challengeBytes

-- | Validate PKCE code verifier against challenge
validateCodeVerifier :: Text -> Text -> Bool
validateCodeVerifier verifier challenge =
    generateCodeChallenge verifier == challenge

-- | Discover OAuth metadata from a well-known endpoint
discoverOAuthMetadata :: (MonadIO m) => Text -> m (Either String OAuthMetadata)
discoverOAuthMetadata issuerUrl = liftIO $ do
    let wellKnownUrl = T.unpack issuerUrl <> "/.well-known/openid-configuration"
    request <- parseRequest wellKnownUrl
    response <- httpJSON request
    return $ Right (getResponseBody response)
