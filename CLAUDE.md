# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

This is a Haskell project using Cabal as its build system.

### Common development commands:
- `cabal build` - Build the project
- `cabal run mcp` - Run the executable
- `cabal test` - Run the test suite
- `cabal repl` - Start a GHCi REPL with the project loaded
- `cabal clean` - Clean build artifacts

## Project Architecture

This is an implementation of the Model Context Protocol (MCP) for Haskell. The project structure follows standard Haskell conventions:

- **src/** - Library modules (MCP.Types, MCP.Protocol, MCP.Server)
- **app/Main.hs** - Executable entry point containing the MCP server implementation
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