# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

This is a Haskell project using Cabal as its build system.

### Common development commands:
- `cabal build` - Build the project (all executables)
- `cabal run mcp` - Run the StdIO MCP server
- `cabal run mcp-http` - Run the HTTP MCP server example
- `cabal test` - Run the test suite
- `cabal repl` - Start a GHCi REPL with the project loaded
- `cabal clean` - Clean build artifacts

## Project Architecture

This is an implementation of the Model Context Protocol (MCP) for Haskell. The project structure follows standard Haskell conventions:

### Core Modules:
- **src/MCP/Types.hs** - Core MCP data types and JSON serialization
- **src/MCP/Protocol.hs** - JSON-RPC protocol messages and types
- **src/MCP/Server.hs** - Core server infrastructure and MCPServer typeclass

### Transport Implementations:
- **src/MCP/Server/StdIO.hs** - StdIO transport for process-based clients
- **src/MCP/Server/HTTP.hs** - HTTP transport following MCP specification

### Application:
- **app/Main.hs** - Example MCP server implementation (StdIO mode)
- **test/Main.hs** - Test suite entry point (currently a placeholder)
- **schema.json** - Contains the complete MCP protocol JSON schema definitions

The project uses GHC2021 language extensions and targets base ^>=4.18.2.1.

## MCP Implementation Notes

The schema.json file contains the full MCP protocol specification including:
- Client/Server request and response types
- Notification types for both directions
- Resource, Tool, and Prompt management
- JSON-RPC message formats
- Capability negotiation structures

When implementing MCP functionality, refer to the schema.json for the exact message formats and type definitions required by the protocol.

## Transport Options

The implementation supports two transport methods:

### StdIO Transport (MCP.Server.StdIO)
- Uses stdin/stdout for JSON-RPC communication  
- Suitable for process-based MCP clients like Claude Desktop
- Default transport used by `cabal run mcp`

### HTTP Transport (MCP.Server.HTTP)
- RESTful HTTP API at `/mcp` endpoint
- Follows the official MCP HTTP transport specification
- Built with Servant and Warp for production use
- Supports JSON-RPC over HTTP POST requests
- Future support planned for Server-Sent Events (SSE)

Both transports use the same MCPServer typeclass, so server implementations work with either transport method.