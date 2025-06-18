# Revision history for mcp

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
