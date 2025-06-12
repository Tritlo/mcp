{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import System.IO (stdin, stdout)

import MCP.Types
import MCP.Protocol
import MCP.Server

-- | Minimal MCP Server implementation
instance MCPServer MCPServerM where
  handleListResources _params = do
    return $ ListResourcesResult { resources = [], nextCursor = Nothing }

  handleReadResource _params = do
    let textContent = TextResourceContents { uri = "example://hello", text = "Hello from MCP Haskell server!", mimeType = Just "text/plain" }
    let content = TextResource textContent
    return $ ReadResourceResult { contents = [content] }

  handleListResourceTemplates _params = do
    return $ ListResourceTemplatesResult { resourceTemplates = [], nextCursor = Nothing }

  handleListPrompts _params = do
    return $ ListPromptsResult { prompts = [], nextCursor = Nothing }

  handleGetPrompt _params = do
    let textContent = TextContent { text = "Hello prompt!" }
    let content = TextContentType textContent
    let message = PromptMessage { role = User, content = content }
    return $ GetPromptResult { messages = [message], description = Nothing }

  handleListTools _params = do
    return $ ListToolsResult { tools = [], nextCursor = Nothing }

  handleCallTool _params = do
    let textContent = TextContent { text = "Tool not implemented" }
    let content = TextContentType textContent
    return $ CallToolResult { content = [content], isError = Nothing }

  handleComplete _params = do
    let completionResult = CompletionResult { values = [], total = Nothing, hasMore = Just True }
    return $ CompleteResult { completion = completionResult, _meta = Nothing }

  handleSetLevel _params = do
    liftIO $ putStrLn "Log level set"

main :: IO ()
main = do
  putStrLn "Starting MCP Haskell Server..."
  
  let serverInfo = Implementation 
        { name = "mcp-haskell-example"
        , version = "0.1.0"
        }
  
  let resourcesCap = ResourcesCapability 
        { subscribe = Just False
        , listChanged = Just False 
        }
  let promptsCap = PromptsCapability 
        { listChanged = Just False 
        }
  let toolsCap = ToolsCapability 
        { listChanged = Just False 
        }
  
  let capabilities = ServerCapabilities
        { resources = Just resourcesCap
        , prompts = Just promptsCap
        , tools = Just toolsCap
        , logging = Nothing
        , experimental = Nothing
        }
  
  let config = ServerConfig
        { configInput = stdin
        , configOutput = stdout
        , configServerInfo = serverInfo
        , configCapabilities = capabilities
        }
  
  putStrLn "Server configured, starting message loop..."
  runServer config