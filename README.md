# MCP-Haskell

A complete implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for Haskell.

## Overview

This project provides a type-safe, comprehensive implementation of the Model Context Protocol in Haskell. MCP is an open protocol that standardizes how applications provide context to Large Language Models (LLMs), enabling AI models to securely connect to data sources and tools.

## Features

- **Complete MCP Protocol Implementation**: All MCP message types, requests, responses, and notifications
- **Type-Safe Design**: Full Haskell type system integration with automatic JSON serialization
- **Multiple Transport Options**: Both StdIO and HTTP transport support
- **MCP Transport Compliance**: HTTP implementation follows the official MCP transport specification
- **Production-Ready HTTP Server**: Configurable OAuth, timing, and security parameters
- **Extensible Server Interface**: Clean typeclass-based API for implementing custom servers
- **Working Example Server**: Demonstrates basic MCP functionality

## Architecture

The implementation is organized into five main modules:

### `MCP.Types`
- Core MCP data types (Content, Resource, Tool, Prompt, etc.)
- Automatic JSON serialization/deserialization via Aeson
- Type-safe mapping of the complete MCP schema

### `MCP.Protocol`
- JSON-RPC message wrappers
- All client and server request/response types
- Notification types for bidirectional communication
- Union types for organizing related messages

### `MCP.Server`
- Core server infrastructure with `MCPServerM` monad stack
- `MCPServer` typeclass for implementing custom servers
- Shared types and utilities for both transport methods

### `MCP.Server.StdIO`
- StdIO transport implementation
- JSON-RPC communication over stdin/stdout
- Suitable for process-based MCP clients

### `MCP.Server.HTTP`
- HTTP transport implementation following MCP specification
- RESTful JSON-RPC API at `/mcp` endpoint
- Built with Servant and Warp for production use
- OAuth 2.0 authentication support (optional)

## Quick Start

### Building

```bash
cabal build
```

### Running the Example Server

**StdIO Mode (default):**
```bash
cabal run mcp
```

The server will start and listen for MCP messages on stdin, responding on stdout.

**HTTP Mode:**
Create a simple HTTP server runner:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import MCP.Server.HTTP
import MCP.Types

main :: IO ()
main = do
  let serverInfo = Implementation "mcp-haskell-http" "0.1.0"
  let capabilities = ServerCapabilities
        { resources = Just $ ResourcesCapability Nothing Nothing
        , tools = Just $ ToolsCapability Nothing
        , prompts = Just $ PromptsCapability Nothing
        , completions = Nothing
        , logging = Nothing
        , experimental = Nothing
        }
  let config = HTTPServerConfig
        { httpPort = 8080
        , httpBaseUrl = "http://localhost:8080"  -- Configure base URL
        , httpServerInfo = serverInfo
        , httpCapabilities = capabilities
        , httpEnableLogging = False
        , httpOAuthConfig = Nothing  -- No OAuth
        , httpJWK = Nothing  -- Auto-generated
        , httpProtocolVersion = "2024-11-05"  -- MCP protocol version
        }
  runServerHTTP config
```

Then compile and run:
```bash
cabal build mcp-http
cabal run mcp-http
```

Or compile directly:
```bash
ghc -o mcp-http MyHTTPServer.hs
./mcp-http
```

The HTTP server will start on port 8080 with the MCP endpoint available at `POST /mcp`.

### OAuth Authentication (HTTP Transport)

The HTTP transport supports MCP-compliant OAuth 2.1 authentication with mandatory PKCE:

**MCP OAuth Requirements:**
- **PKCE Required**: All OAuth flows MUST use PKCE (Proof Key for Code Exchange)
- **HTTPS Required**: OAuth endpoints must use HTTPS in production
- **Grant Types**: Supports Authorization Code (user flows) and Client Credentials (app-to-app)
- **Token Format**: Bearer tokens in Authorization header only (never in query strings)

**OAuth Endpoints:**
- **Metadata Discovery**: `/.well-known/oauth-authorization-server` - Returns OAuth server metadata
- **Dynamic Registration**: `/register` - Allows clients to register dynamically
- **Authorization**: `/authorize` - Initiates authorization flow with PKCE
- **Token Exchange**: `/token` - Exchanges authorization code for access token

**Token Format**: The server generates proper JWT tokens using servant-auth-server, preventing authentication loops that can occur with simple UUID-based tokens.

**Complete OAuth Flow:**

1. **Discovery**: Client discovers OAuth metadata
   ```bash
   curl http://localhost:8080/.well-known/oauth-authorization-server
   ```

2. **Registration**: Client registers dynamically
   ```bash
   curl -X POST http://localhost:8080/register \
     -H "Content-Type: application/json" \
     -d '{
       "client_name": "My MCP Client",
       "redirect_uris": ["http://localhost:3000/callback"],
       "grant_types": ["authorization_code", "refresh_token"],
       "response_types": ["code"],
       "token_endpoint_auth_method": "none"
     }'
   ```

3. **Authorization**: User authorizes with PKCE
   ```
   http://localhost:8080/authorize?
     response_type=code&
     client_id=CLIENT_ID&
     redirect_uri=http://localhost:3000/callback&
     code_challenge=CHALLENGE&
     code_challenge_method=S256&
     scope=mcp:read%20mcp:write
   ```

4. **Token Exchange**: Exchange code for token
   ```bash
   curl -X POST http://localhost:8080/token \
     -H "Content-Type: application/x-www-form-urlencoded" \
     -d "grant_type=authorization_code&code=AUTH_CODE&code_verifier=VERIFIER"
   ```

5. **API Access**: Use token for authenticated requests
   ```bash
   curl -X POST http://localhost:8080/mcp \
     -H "Authorization: Bearer ACCESS_TOKEN" \
     -H "Content-Type: application/json" \
     -d '{"jsonrpc":"2.0","id":1,"method":"ping"}'
   ```

**Error Responses (MCP-compliant):**
- `401 Unauthorized`: Invalid or expired token
- `403 Forbidden`: Insufficient permissions
- `400 Bad Request`: Malformed request

### Implementing a Custom Server

The same server implementation works for both transport methods:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import MCP.Server
import MCP.Server.StdIO  -- For StdIO transport
import MCP.Server.HTTP   -- For HTTP transport
import MCP.Types
import MCP.Protocol

instance MCPServer MCPServerM where
  handleListTools _params = do
    let tool = Tool
          { name = "calculator"
          , description = Just "A simple calculator"
          , inputSchema = -- JSON schema for tool parameters
          , annotations = Nothing
          }
    return $ ListToolsResult [tool] Nothing
    
  handleCallTool params = do
    -- Implement your tool logic here
    let result = TextContentType $ TextContent "Result: 42"
    return $ CallToolResult [result] Nothing
    
  -- Implement other required methods...

-- StdIO version
runStdIO :: IO ()
runStdIO = do
  let config = ServerConfig
        { configInput = stdin
        , configOutput = stdout
        , configServerInfo = Implementation "my-server" "1.0.0"
        , configCapabilities = serverCapabilities
        }
  MCP.Server.StdIO.runServer config

-- HTTP version  
runHTTP :: IO ()
runHTTP = do
  let config = HTTPServerConfig
        { httpPort = 8080
        , httpServerInfo = Implementation "my-server" "1.0.0"
        , httpCapabilities = serverCapabilities
        , httpEnableLogging = False
        , httpOAuthConfig = Nothing  -- or Just oauthConfig for OAuth
        }
  runServerHTTP config
```

## MCP Protocol Support

This implementation supports the complete MCP protocol specification:

### ✅ Implemented Features

- **Initialization**: Protocol version negotiation and capability exchange
- **Resources**: Expose data and content that can be read by LLMs
- **Tools**: Functions that LLMs can execute
- **Prompts**: Pre-written prompt templates with arguments
- **Completion**: Auto-completion for resource URIs, prompt arguments, etc.
- **Logging**: Configurable logging levels
- **Notifications**: Bidirectional event notifications

### Core Operations

| Operation | Description | Status |
|-----------|-------------|--------|
| `initialize` | Start session and negotiate capabilities | ✅ |
| `ping` | Health check | ✅ |
| `resources/list` | List available resources | ✅ |
| `resources/read` | Read resource contents | ✅ |
| `prompts/list` | List available prompts | ✅ |
| `prompts/get` | Get prompt with arguments | ✅ |
| `tools/list` | List available tools | ✅ |
| `tools/call` | Execute a tool | ✅ |
| `completion/complete` | Auto-completion | ✅ |
| `logging/setLevel` | Set logging level | ✅ |

## Project Structure

```
src/
├── MCP/
│   ├── Types.hs          # Core MCP data types
│   ├── Protocol.hs       # JSON-RPC protocol messages
│   ├── Server.hs         # Core server infrastructure
│   └── Server/
│       ├── StdIO.hs      # StdIO transport implementation
│       └── HTTP.hs       # HTTP transport implementation

app/
└── Main.hs               # Example MCP server (StdIO mode)

test/
└── Main.hs               # Test suite (placeholder)
```

## Development

### Dependencies

**Core Dependencies:**
- **aeson**: JSON serialization/deserialization
- **text**: Text processing
- **containers**: Map and other container types
- **bytestring**: Binary data handling
- **mtl/transformers**: Monad transformers for MCPServerM

**HTTP Server Dependencies:**
- **warp**: High-performance HTTP server
- **servant-server**: Type-safe web API framework  
- **wai**: Web application interface
- **http-types**: HTTP status codes and headers
- **servant-auth**: JWT authentication for Servant
- **servant-auth-server**: Server-side JWT implementation
- **jose**: JSON Object Signing and Encryption
- **cryptonite**: Cryptographic primitives for PKCE
- **base64-bytestring**: Base64 encoding for PKCE challenges
- **http-conduit**: HTTP client for OAuth metadata discovery

### Testing with MCP Clients

**StdIO Transport (for process-based clients):**

1. Build the server: `cabal build`
2. Configure your MCP client to use: `cabal run mcp`
3. The server communicates via JSON-RPC over stdin/stdout

**HTTP Transport (for web-based clients):**

1. Build and run HTTP server: `runServerHTTP config`
2. Client connects to `POST http://localhost:8080/mcp`
3. Send JSON-RPC requests with `Content-Type: application/json`
4. Supports both single responses and future SSE streaming

### Example MCP Client Configuration

For Claude Desktop, add to your config file (location varies by OS):

```json
{
  "mcpServers": {
    "haskell-mcp": {
      "command": "cabal",
      "args": ["run", "mcp"],
      "cwd": "/absolute/path/to/mcp-haskell"
    }
  }
}
```

**Configuration file locations:**
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json` 
- **Linux**: `~/.config/claude/claude_desktop_config.json`

See the [`examples/`](examples/) directory for more detailed configuration examples and setup instructions.

## Implementation Notes

### Type Safety

The implementation leverages Haskell's type system to ensure protocol compliance:

- All MCP message types are statically typed
- JSON schema validation through type structure
- Compile-time guarantees for message format correctness

### Content Types

MCP supports multiple content types with proper type wrappers:

```haskell
-- Text content
TextContentType $ TextContent "Hello world!"

-- Image content  
ImageContentType $ ImageContent "data:image/png;base64,..." "image/png"

-- Resource content
TextResource $ TextResourceContents "file:///example.txt" "content" Nothing
```

### Error Handling

The server includes comprehensive error handling:

- JSON parsing errors
- Invalid method names
- Missing or malformed parameters
- Server initialization requirements

## Contributing

This is the first known implementation of MCP for Haskell. Contributions are welcome!

Areas for improvement:
- Server-Sent Events (SSE) support for HTTP transport
- WebSocket transport implementation
- More comprehensive example servers
- Performance optimizations
- Enhanced error messages
- Documentation and tutorials

## License

MIT License - see LICENSE file for details.

## References

- [Model Context Protocol Specification](https://modelcontextprotocol.io/)
- [MCP TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [Official MCP Documentation](https://modelcontextprotocol.io/introduction)
