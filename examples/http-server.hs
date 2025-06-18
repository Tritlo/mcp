{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Example HTTP MCP Server
-- 
-- This example demonstrates how to run the MCP server over HTTP transport.
-- The server will expose the MCP API at POST /mcp
--
-- To test:
-- 1. Compile: cabal build mcp-http
-- 2. Run: cabal run mcp-http
-- 3. Send JSON-RPC requests to: http://localhost:<port>/mcp
--
-- Example request:
-- curl -X POST http://localhost:8080/mcp \
--   -H "Content-Type: application/json" \
--   -d '{"jsonrpc":"2.0","id":1,"method":"ping"}'
--
-- Command line options:
-- cabal run mcp-http -- --port 8080 --log

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Options.Applicative

import MCP.Protocol hiding (CompletionResult)
import MCP.Protocol qualified as Protocol
import MCP.Server
import MCP.Server.HTTP
import MCP.Types

-- | Command line options
data Options = Options
    { optPort :: Int
    , optEnableLogging :: Bool
    }
    deriving (Show)

-- | Parser for command line options
optionsParser :: Parser Options
optionsParser = Options
    <$> option auto
        ( long "port"
       <> short 'p'
       <> metavar "PORT"
       <> Options.Applicative.value 8080
       <> help "Port to run the HTTP server on (default: 8080)"
        )
    <*> switch
        ( long "log"
       <> short 'l'
       <> help "Enable request/response logging"
        )

-- | Full parser with help
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Run an MCP server over HTTP transport"
   <> header "mcp-http - HTTP MCP Server Example"
    )

-- | Example MCP Server implementation (copied from Main.hs)
instance MCPServer MCPServerM where
    handleListResources _params = do
        return $ ListResourcesResult{resources = [], nextCursor = Nothing, _meta = Nothing}

    handleReadResource _params = do
        let textContent = TextResourceContents{uri = "example://hello", text = "Hello from MCP Haskell HTTP server!", mimeType = Just "text/plain"}
        let content = TextResource textContent
        return $ ReadResourceResult{contents = [content], _meta = Nothing}

    handleListResourceTemplates _params = do
        return $ ListResourceTemplatesResult{resourceTemplates = [], nextCursor = Nothing, _meta = Nothing}

    handleListPrompts _params = do
        return $ ListPromptsResult{prompts = [], nextCursor = Nothing, _meta = Nothing}

    handleGetPrompt _params = do
        let textContent = TextContent{text = "Hello HTTP prompt!", textType = "text", annotations = Nothing}
        let content = TextContentType textContent
        let message = PromptMessage{role = User, content = content}
        return $ GetPromptResult{messages = [message], description = Nothing, _meta = Nothing}

    handleListTools _params = do
        let getCurrentDateTool =
                Tool
                    { name = "getCurrentDate"
                    , description = Just "Get the current date and time via HTTP"
                    , inputSchema = InputSchema "object" Nothing Nothing
                    , annotations = Nothing
                    }
        return $ ListToolsResult{tools = [getCurrentDateTool], nextCursor = Nothing, _meta = Nothing}

    handleCallTool CallToolParams{name = toolName} = do
        case toolName of
            "getCurrentDate" -> do
                currentTime <- liftIO getCurrentTime
                let dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC (via HTTP)" currentTime
                let textContent = TextContent{text = T.pack dateStr, textType = "text", annotations = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], isError = Nothing, _meta = Nothing}
            _ -> do
                let textContent = TextContent{text = "Tool not found", textType = "text", annotations = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], isError = Just True, _meta = Nothing}

    handleComplete _params = do
        let completionResult = Protocol.CompletionResult{values = [], total = Nothing, hasMore = Just True}
        return $ CompleteResult{completion = completionResult, _meta = Nothing}

    handleSetLevel _params = do
        liftIO $ putStrLn "Log level set via HTTP"

main :: IO ()
main = do
    Options{..} <- execParser opts
    
    putStrLn "Starting MCP Haskell HTTP Server..."
    putStrLn $ "Port: " ++ show optPort
    when optEnableLogging $ putStrLn "Request/Response logging: enabled"

    let serverInfo =
            Implementation
                { name = "mcp-haskell-http-example"
                , version = "0.1.0"
                }

    let resourcesCap =
            ResourcesCapability
                { subscribe = Just False
                , listChanged = Just False
                }
    let promptsCap =
            PromptsCapability
                { listChanged = Just False
                }
    let toolsCap =
            ToolsCapability
                { listChanged = Just False
                }

    let capabilities =
            ServerCapabilities
                { resources = Just resourcesCap
                , prompts = Just promptsCap
                , tools = Just toolsCap
                , completions = Nothing
                , logging = Nothing
                , experimental = Nothing
                }

    let config =
            HTTPServerConfig
                { httpPort = optPort
                , httpServerInfo = serverInfo
                , httpCapabilities = capabilities
                , httpEnableLogging = optEnableLogging
                }

    putStrLn $ "HTTP server configured, starting on port " ++ show optPort ++ "..."
    putStrLn $ "MCP endpoint available at: POST http://localhost:" ++ show optPort ++ "/mcp"
    putStrLn ""
    putStrLn "Example test command:"
    putStrLn $ "curl -X POST http://localhost:" ++ show optPort ++ "/mcp \\"
    putStrLn "  -H \"Content-Type: application/json\" \\"
    putStrLn "  -d '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}'"
    putStrLn ""
    
    runServerHTTP config