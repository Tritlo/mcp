# MCP-Haskell

A complete implementation of the [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) for Haskell.

## Overview

This project provides a type-safe, comprehensive implementation of the Model Context Protocol in Haskell. MCP is an open protocol that standardizes how applications provide context to Large Language Models (LLMs), enabling AI models to securely connect to data sources and tools.

## Features

- **Complete MCP Protocol Implementation**: All MCP message types, requests, responses, and notifications
- **Type-Safe Design**: Full Haskell type system integration with automatic JSON serialization
- **JSON-RPC Transport**: Standard I/O transport for connecting to MCP hosts
- **Extensible Server Interface**: Clean typeclass-based API for implementing custom servers
- **Working Example Server**: Demonstrates basic MCP functionality

## Architecture

The implementation is organized into three main modules:

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
- Server infrastructure with `MCPServerM` monad stack
- `MCPServer` typeclass for implementing custom servers
- Request routing and error handling
- JSON-RPC transport layer over stdin/stdout

## Quick Start

### Building

```bash
cabal build
```

### Running the Example Server

```bash
cabal run mcp
```

The server will start and listen for MCP messages on stdin, responding on stdout.

### Implementing a Custom Server

```haskell
{-# LANGUAGE OverloadedStrings #-}

import MCP.Server
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

main :: IO ()
main = do
  let config = ServerConfig
        { configInput = stdin
        , configOutput = stdout
        , configServerInfo = Implementation "my-server" "1.0.0"
        , configCapabilities = serverCapabilities
        }
  runServer config
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
│   ├── Types.hs      # Core MCP data types
│   ├── Protocol.hs   # JSON-RPC protocol messages  
│   └── Server.hs     # Server infrastructure
└── MyLib.hs          # Placeholder library module

app/
└── Main.hs           # Example MCP server implementation

test/
└── Main.hs           # Test suite (placeholder)
```

## Development

### Dependencies

- **aeson**: JSON serialization/deserialization
- **text**: Text processing
- **containers**: Map and other container types
- **bytestring**: Binary data handling
- **mtl/transformers**: Monad transformers for MCPServerM

### Testing with MCP Clients

To test with Claude Desktop or other MCP clients:

1. Build the server: `cabal build`
2. Configure your MCP client to use: `cabal run mcp`
3. The server communicates via JSON-RPC over stdin/stdout

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
- Additional transport mechanisms (HTTP, WebSocket)
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