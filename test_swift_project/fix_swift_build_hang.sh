#!/bin/bash
# fix_swift_build_hang.sh
# Script to fix Swift 6.1 hanging issue on Ubuntu 24.04
# Usage: ./fix_swift_build_hang.sh [path_to_package]

set -e

# Default to current directory if no path provided
PACKAGE_PATH=${1:-.}
cd "$PACKAGE_PATH"

# Check if .build directory exists
if [ -d ".build" ]; then
    echo "Found .build directory. Checking for lock files..."
    
    # Check for and remove lock files
    if [ -f ".build/lock" ] || [ -f ".build/workspace-state.json.lock" ]; then
        echo "Lock files found, removing them..."
        rm -f .build/lock .build/workspace-state.json.lock
    fi
    
    # Verify permissions on .build directory
    echo "Fixing permissions on .build directory..."
    chmod -R u+rw .build
    
    # Check if build.db exists (SQLite database used by SwiftPM)
    if [ -f ".build/build.db" ] || [ -f ".build/build.db-wal" ] || [ -f ".build/build.db-shm" ]; then
        echo "Build database found, removing SQLite journal files..."
        rm -f .build/build.db-wal .build/build.db-shm
    fi
    
    # Clean workspace state but preserve dependencies
    if [ -f ".build/workspace-state.json" ]; then
        echo "Resetting workspace state..."
        mv .build/workspace-state.json .build/workspace-state.json.bak
    fi
    
    echo "Cleaning build artifacts but preserving downloaded dependencies..."
    find .build -path "*.build/*/build.db*" -delete
    find .build -name "*.swiftmodule" -delete
    find .build -path "*.build/release" -type d -exec rm -rf {} \; 2>/dev/null || true
    find .build -path "*.build/debug" -type d -exec rm -rf {} \; 2>/dev/null || true
    
    echo "Clean-up completed. Try running 'swift build' again."
else
    echo "No .build directory found. Please run 'swift build' first."
fi

echo ""
echo "If the issue persists, you may need to completely remove .build directory:"
echo "rm -rf .build && swift build -c release"
echo ""
echo "For a more targeted solution, try running:"
echo "swift package clean && swift build -c release" 