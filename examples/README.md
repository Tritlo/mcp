# MCP Configuration Examples and Usage

This directory contains example configuration files for connecting MCP clients to the Haskell MCP server, as well as example implementations demonstrating different transport methods.

## Claude Desktop Configuration

Claude Desktop reads its MCP server configuration from a JSON file. The location depends on your operating system:

### Configuration File Locations

- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
- **Linux**: `~/.config/claude/claude_desktop_config.json`

### Basic Configuration

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

### Development Setup

For development with Cabal:

```json
{
  "mcpServers": {
    "haskell-mcp-dev": {
      "command": "cabal",
      "args": ["run", "mcp"],
      "cwd": "/home/user/projects/mcp-haskell",
      "env": {
        "GHC_ENVIRONMENT": "-"
      }
    }
  }
}
```

### Production Setup

For production with a compiled binary:

```json
{
  "mcpServers": {
    "haskell-mcp-prod": {
      "command": "/usr/local/bin/mcp",
      "args": [],
      "cwd": "/opt/mcp-servers"
    }
  }
}
```

### Stack-based Setup

If using Stack instead of Cabal:

```json
{
  "mcpServers": {
    "haskell-mcp-stack": {
      "command": "stack",
      "args": ["exec", "mcp"],
      "cwd": "/path/to/mcp-haskell"
    }
  }
}
```

## Configuration Fields

- **command**: The executable to run (cabal, stack, or direct binary path)
- **args**: Arguments passed to the command
- **cwd**: Working directory (should be the project root for development)
- **env**: Environment variables (optional)

## Testing the Configuration

1. Save the configuration to the appropriate location for your OS
2. Restart Claude Desktop
3. Start a new conversation
4. The Haskell MCP server should appear in the available tools/context

## Troubleshooting

### Common Issues

1. **Path Problems**: Ensure `cwd` points to the correct project directory
2. **Permission Issues**: Make sure the command is executable
3. **Build Issues**: Run `cabal build` first to ensure the project compiles
4. **Port Conflicts**: Each server needs a unique name in the configuration

### Debugging

To test the server manually:

```bash
cd /path/to/mcp-haskell
cabal run mcp
```

The server should start and wait for JSON-RPC messages on stdin.

### Logs

Claude Desktop logs can help debug connection issues:

- **macOS**: `~/Library/Logs/Claude/`
- **Windows**: `%LOCALAPPDATA%\Claude\logs\`
- **Linux**: `~/.local/share/claude/logs/`

## Example Usage

Once configured, you can:

1. **List Resources**: Ask Claude to show available resources
2. **Read Content**: Request specific resource content
3. **Use Tools**: Execute tools provided by the server
4. **Get Prompts**: Use pre-defined prompt templates

The Haskell MCP server provides basic examples of each capability that you can extend for your specific use case.

---

## HTTP Server Example

**File:** `http-server.hs`

Demonstrates how to run an MCP server using HTTP transport instead of StdIO.

### Building and Running

**Using Cabal (recommended):**
```bash
# From the project root
cabal build mcp-http
cabal run mcp-http
```

**Manual compilation:**
```bash
# From the project root
ghc -package-env=. -o examples/http-server examples/http-server.hs
cd examples
./http-server
```

**Using cabal exec:**
```bash
cabal exec ghc -- -package-env=. -o examples/http-server examples/http-server.hs
cd examples  
./http-server
```

### Testing the HTTP Server

Once running, the server exposes the MCP API at `POST http://localhost:8080/mcp`.

**Test with curl:**

```bash
# Ping test
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"ping"}'

# Initialize the server
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc":"2.0",
    "id":1,
    "method":"initialize",
    "params":{
      "protocolVersion":"2024-11-05",
      "capabilities":{},
      "clientInfo":{"name":"test-client","version":"1.0.0"}
    }
  }'

# List available tools
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":2,"method":"tools/list"}'

# Call the getCurrentDate tool
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc":"2.0",
    "id":3,
    "method":"tools/call",
    "params":{"name":"getCurrentDate"}
  }'
```

**Test with HTTPie:**

```bash
# Ping test
http POST localhost:8080/mcp jsonrpc=2.0 id:=1 method=ping

# Initialize
http POST localhost:8080/mcp \
  jsonrpc=2.0 id:=1 method=initialize \
  params:='{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}'

# List tools
http POST localhost:8080/mcp jsonrpc=2.0 id:=2 method=tools/list

# Call tool
http POST localhost:8080/mcp \
  jsonrpc=2.0 id:=3 method=tools/call \
  params:='{"name":"getCurrentDate"}'
```

## Key Differences: StdIO vs HTTP

| Aspect | StdIO Transport | HTTP Transport |
|--------|----------------|----------------|
| **Client Integration** | Process-based (stdin/stdout) | HTTP clients, web apps |
| **Message Format** | Line-delimited JSON-RPC | HTTP POST with JSON body |
| **Server Lifecycle** | Managed by client process | Independent HTTP service |
| **Debugging** | Log to stderr | HTTP access logs |
| **Scalability** | One client per process | Multiple concurrent clients |
| **Network** | Local only | Network accessible |

## Implementation Notes

- Both transports use the same `MCPServer` typeclass implementation
- Server logic is identical between StdIO and HTTP modes
- HTTP transport follows the MCP specification for streamable HTTP
- Future versions will support Server-Sent Events (SSE) for bidirectional communication

## Error Handling

The HTTP server returns appropriate HTTP status codes:

- **200 OK**: Successful JSON-RPC response
- **400 Bad Request**: Invalid JSON or malformed JSON-RPC
- **500 Internal Server Error**: Server-side processing errors

JSON-RPC errors are returned within the 200 response body following the JSON-RPC 2.0 specification.