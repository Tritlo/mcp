#!/bin/bash

# Test script for MCP Haskell servers (stdio and http).
# This script sends basic MCP messages to test server functionality

# Suppress job control messages
# set +m

# Build the servers first
cabal build -v0 lib:mcp mcp-http mcp-stdio

# Create a temporary file for test messages
TEST_FILE=$(mktemp)

# Write test JSON-RPC messages (each on its own line as required by JSON-RPC)
cat > "$TEST_FILE" <<EOF
{"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2024-11-05", "capabilities": {"roots": {"listChanged": true}}, "clientInfo": {"name": "TestClient", "title": "Test Client Display Name", "version": "1.0.0"}}}
{"jsonrpc": "2.0", "id": 2, "method": "ping"}
{"jsonrpc": "2.0", "id": 3, "method": "resources/list"}
{"jsonrpc": "2.0", "id": 4, "method": "prompts/list"}
{"jsonrpc": "2.0", "id": 5, "method": "tools/list"}
EOF

NUM_TESTS=$(wc -l $TEST_FILE | awk '{print $1}')

# Create a temporary file for the server PID
SERVER_PID_FILE=$(mktemp)

function cleanup() {
    SERVER_PID=$(cat $SERVER_PID_FILE)
    { kill $SERVER_PID && wait $SERVER_PID; } 2> /dev/null

    # Clean up temporary files
    rm "$TEST_FILE" 2> /dev/null
    rm "$SERVER_PID_FILE" 2> /dev/null

    return 0
}

function test_stdio_server() {
    local LOG_FILE=$(mktemp)

    cabal run -v0 mcp-stdio -- --log < $TEST_FILE 2>&1 > $LOG_FILE
    
    # FAILURES=$(($NUM_TESTS - $(grep -c '"jsonrpc":"2.0"' $LOG) + 1))
    # +1 to account for the example message the stdio server prints on startup
    return $(($NUM_TESTS - $(grep -c '"jsonrpc":"2.0"' $LOG_FILE) + 1))
}

function test_http_server() {
    local LOG_FILE=$(mktemp)

    cabal run -v0 mcp-http -- --port 8080 --log 2>&1 > /dev/null &
    SERVER_PID=$!
    echo $SERVER_PID > $SERVER_PID_FILE
    sleep 1 # wait for the server to start

    # Test all methods
    FAILURES=$NUM_TESTS
    while IFS= read -r message; do
        curl -s -X POST -H "Content-Type: application/json" -d "$message" http://localhost:8080/mcp 2>&1 > $LOG_FILE
        if grep -q '"jsonrpc":"2.0"' $LOG_FILE; then
            FAILURES=$((FAILURES-1))
        else
            echo "❌ Unexpected response to $message:"
            echo "----"
            cat $LOG_FILE
            echo "----"
        fi
    done < "$TEST_FILE"

    return $FAILURES
}

function run_test() {
    SERVER_NAME=$1
    test_${SERVER_NAME}_server
    RESULT=$?
    if [ $RESULT -eq 0 ]; then
        echo "🎉 [$NUM_TESTS/$NUM_TESTS] MCP Haskell $SERVER_NAME server"
    else
        echo "❌ [$((NUM_TESTS - RESULT))/$NUM_TESTS] MCP Haskell $SERVER_NAME server"
        return 1
    fi
}

# Run tests
run_test "http"
HTTP_RESULT=$?
run_test "stdio"
STDIO_RESULT=$?

cleanup

exit $HTTP_RESULT || $STDIO_RESULT
