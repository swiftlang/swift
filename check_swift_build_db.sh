#!/bin/bash
# check_swift_build_db.sh
# Script to check and repair Swift Package Manager's SQLite database
# Usage: ./check_swift_build_db.sh [path_to_package]

set -e

# Check if sqlite3 is installed
if ! command -v sqlite3 &> /dev/null; then
    echo "Error: sqlite3 is not installed. Please install it first:"
    echo "  sudo apt-get install sqlite3"
    exit 1
fi

# Default to current directory if no path provided
PACKAGE_PATH=${1:-.}
cd "$PACKAGE_PATH"

# Check if .build directory exists
if [ ! -d ".build" ]; then
    echo "No .build directory found. Please run 'swift build' first."
    exit 1
fi

# Check if build.db exists
BUILD_DB=".build/build.db"
if [ ! -f "$BUILD_DB" ]; then
    echo "No build.db found in .build directory."
    exit 1
fi

echo "Checking Swift Package Manager's SQLite database..."

# Check for WAL and SHM files (SQLite journal files)
if [ -f "$BUILD_DB-wal" ] || [ -f "$BUILD_DB-shm" ]; then
    echo "Found SQLite journal files. These could be causing the hang."
    
    # Create backup before operations
    echo "Creating backup of build database..."
    cp "$BUILD_DB" "$BUILD_DB.bak"
    
    # Remove journal files
    echo "Removing SQLite journal files..."
    rm -f "$BUILD_DB-wal" "$BUILD_DB-shm"
fi

# Check if the database is corrupt
echo "Running SQLite integrity check..."
INTEGRITY=$(sqlite3 "$BUILD_DB" "PRAGMA integrity_check;" || echo "ERROR")

if [ "$INTEGRITY" = "ok" ]; then
    echo "Database integrity check passed."
else
    echo "Database integrity check failed! Attempting to recover..."
    
    # Try to recover the database
    echo "Creating a new database and transferring schema..."
    NEW_DB=".build/build_new.db"
    
    # Dump the schema (without data) and apply to new database
    sqlite3 "$BUILD_DB" ".schema" | sqlite3 "$NEW_DB"
    
    # Replace the old database with the new one
    mv "$NEW_DB" "$BUILD_DB"
    echo "Database recovered with empty schema. You'll need to rebuild."
fi

echo "Done. Try running 'swift build -c release' now."
echo "If the issue persists, consider using the fix_swift_build_hang.sh script"
echo "or removing the .build directory entirely: rm -rf .build" 