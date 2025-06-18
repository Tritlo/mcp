# Revision history for mcp

## 0.3.0.0 -- 2025-06-18

* **Protocol Update**: Full support for MCP protocol version 2025-06-18 with complete schema compliance
* **Major Type System Updates**:
  - Add BaseMetadata interface with name/title distinction for consistent naming across all types
  - Rename Content to ContentBlock for consistency with official schema
  - Add ResourceLink content type for referencing resources in prompts and tool results
  - Create restricted SamplingContent type (text, image, audio only) for SamplingMessage
  - Fix ResourceTemplateReference to use correct type "ref/resource" with uri field
* **Enhanced Features**:
  - Add tool output schema support (outputSchema field in Tool type)
  - Add elicitation system for interactive user input forms with primitive schema validation
  - Add enhanced metadata with lastModified timestamps and _meta fields throughout
  - Add context parameter to completion requests for better autocompletion
  - Add structured content support for tool results (structuredContent field)
  - Add instructions field to InitializeResult for server usage hints
* **Schema Compliance**:
  - Update all types to include optional _meta fields where specified in schema
  - Fix Reference union to include ResourceTemplateReference instead of ResourceReference
  - Ensure CreateMessageResult properly extends SamplingMessage with restricted content
  - Add ResourceTemplateReference type with correct schema-compliant structure
  - Complete type exports for all new schema elements
* **Documentation and Examples**:
  - Add comprehensive documentation for CompleteParams with field descriptions
  - Update all example code to include new required fields (_meta, title, outputSchema, etc.)
  - Update README with latest protocol version information
  - Enhanced Cabal description highlighting 2025-06-18 protocol support
* **BREAKING CHANGE**: Various type signatures updated to match new protocol
  - Content renamed to ContentBlock (legacy alias provided for backward compatibility)
  - SamplingMessage content restricted to TextContent | ImageContent | AudioContent
  - ResourceTemplateReference structure changed to match schema
  - Reference union now uses ResourceTemplateReference instead of ResourceReference

## 0.2.0.0 -- 2025-01-18

* Add HTTP transport support following MCP specification
* Implement full OAuth 2.0 authorization flow with mandatory PKCE
  - OAuth metadata discovery at /.well-known/oauth-authorization-server
  - Dynamic client registration at /register endpoint
  - /authorize endpoint for authorization requests with PKCE validation
  - /token endpoint for authorization code and refresh token grants
  - In-memory storage for authorization codes, tokens, and registered clients
  - Automatic user approval for demo mode
  - Support for public clients (no client secret required)
* Add OAuth 2.1 authentication module (MCP.Server.Auth)
  - JWT token validation and introspection
  - PKCE code verifier/challenge generation and validation (S256 method)
  - OAuth metadata discovery with ToJSON/FromJSON instances
  - Support for multiple OAuth providers (Google, GitHub, custom)
  - Token expiration and not-before time validation
* OAuth implementation features:
  - Returns empty string for client_secret (public clients)
  - Validates redirect URIs and client IDs
  - 10-minute authorization code expiry
  - 1-hour access token validity
  - Refresh token support with rotation
  - Proper JWT token generation using servant-auth-server
  - Fixed authentication loop issue by replacing UUID tokens with JWT
* Add example OAuth resources:
  - OAuth demo client script (examples/oauth-client-demo.sh)
  - OAuth metadata test script (test-oauth-metadata.sh)
  - Updated HTTP server example with --oauth flag
* Fix all compiler warnings with -Wall enabled
* Clean up unused imports and parameters
* Add uuid dependency for token generation
* **BREAKING CHANGE**: Refactor HTTP server to be production-ready:
  - Extract hardcoded demo values into configurable parameters
  - Add `httpBaseUrl` and `httpProtocolVersion` to HTTPServerConfig
  - Expand OAuthConfig with timing, OAuth parameters, and demo settings
  - Move demo-specific configuration to examples/http-server.hs
  - Add `defaultDemoOAuthConfig` helper for testing
  - Make server library more robust and configurable for production use
* Package metadata cleanup and code formatting improvements
* Remove unused MyLib module
* Add MCP configuration and examples
* Published to Hackage

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
