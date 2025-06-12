{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (stdin, stdout)

import MCP.Protocol
import MCP.Server
import MCP.Types

-- | Minimal MCP Server implementation
instance MCPServer MCPServerM where
    handleListResources _params = do
        return $ ListResourcesResult{resources = [], nextCursor = Nothing, _meta = Nothing}

    handleReadResource _params = do
        let textContent = TextResourceContents{uri = "example://hello", text = "Hello from MCP Haskell server!", mimeType = Just "text/plain"}
        let content = TextResource textContent
        return $ ReadResourceResult{contents = [content], _meta = Nothing}

    handleListResourceTemplates _params = do
        return $ ListResourceTemplatesResult{resourceTemplates = [], nextCursor = Nothing, _meta = Nothing}

    handleListPrompts _params = do
        return $ ListPromptsResult{prompts = [], nextCursor = Nothing, _meta = Nothing}

    handleGetPrompt _params = do
        let textContent = TextContent{text = "Hello prompt!", textType = "text", annotations = Nothing}
        let content = TextContentType textContent
        let message = PromptMessage{role = User, content = content}
        return $ GetPromptResult{messages = [message], description = Nothing, _meta = Nothing}

    handleListTools _params = do
        let getCurrentDateTool =
                Tool
                    { name = "getCurrentDate"
                    , description = Just "Get the current date and time"
                    , inputSchema = InputSchema "object" Nothing Nothing
                    , annotations = Nothing
                    }
        return $ ListToolsResult{tools = [getCurrentDateTool], nextCursor = Nothing, _meta = Nothing}

    handleCallTool CallToolParams{name = toolName} = do
        case toolName of
            "getCurrentDate" -> do
                currentTime <- liftIO getCurrentTime
                let dateStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" currentTime
                let textContent = TextContent{text = T.pack dateStr, textType = "text", annotations = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], isError = Nothing, _meta = Nothing}
            _ -> do
                let textContent = TextContent{text = "Tool not found", textType = "text", annotations = Nothing}
                let content = TextContentType textContent
                return $ CallToolResult{content = [content], isError = Just True, _meta = Nothing}

    handleComplete _params = do
        let completionResult = CompletionResult{values = [], total = Nothing, hasMore = Just True}
        return $ CompleteResult{completion = completionResult, _meta = Nothing}

    handleSetLevel _params = do
        liftIO $ putStrLn "Log level set"

main :: IO ()
main = do
    putStrLn "Starting MCP Haskell Server..."

    let serverInfo =
            Implementation
                { name = "mcp-haskell-example"
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
            ServerConfig
                { configInput = stdin
                , configOutput = stdout
                , configServerInfo = serverInfo
                , configCapabilities = capabilities
                }

    putStrLn "Server configured, starting message loop..."
    runServer config
