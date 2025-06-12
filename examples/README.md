# MCP Configuration Examples

This directory contains example configuration files for connecting MCP clients to the Haskell MCP server.

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