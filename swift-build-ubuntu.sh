#!/bin/bash
# swift-build-ubuntu.sh
# A wrapper script for swift build to avoid hanging issues on Ubuntu 24.04
# Usage: ./swift-build-ubuntu.sh [swift_build_arguments]

set -e

# Store the original arguments
BUILD_ARGS="$@"

# Determine the Swift executable path
if [ -x "$HOME/swift-6.1-RELEASE-ubuntu24.04/usr/bin/swift" ]; then
    SWIFT_PATH="$HOME/swift-6.1-RELEASE-ubuntu24.04/usr/bin/swift"
else
    SWIFT_PATH=$(which swift)
fi

echo "Using Swift at: $SWIFT_PATH"
echo "Swift version: $($SWIFT_PATH --version | head -n 1)"

# Function to check if build is hanging
check_build_hanging() {
    local pid=$1
    local start_time=$SECONDS
    local last_log_size=0
    local current_log_size=0
    
    # Check every 5 seconds
    while kill -0 $pid 2>/dev/null; do
        sleep 5
        
        # Check if the log file size has changed
        if [ -f "build.log" ]; then
            current_log_size=$(stat -c%s "build.log" 2>/dev/null || stat -f%z "build.log" 2>/dev/null)
            
            # If the build is running for more than 30 seconds and log size hasn't changed
            if [ $((SECONDS - start_time)) -gt 30 ] && [ $current_log_size -eq $last_log_size ]; then
                echo "Build appears to be hanging (no output for 30+ seconds). Terminating..."
                kill $pid
                return 1
            fi
            
            last_log_size=$current_log_size
        fi
    done
    
    return 0
}

# Try building normally first
echo "Attempting standard build: $SWIFT_PATH build $BUILD_ARGS"
$SWIFT_PATH build $BUILD_ARGS > build.log 2>&1 &
BUILD_PID=$!

if check_build_hanging $BUILD_PID; then
    echo "Build completed successfully!"
    cat build.log
    rm build.log
    exit 0
fi

# If we reach here, the build was terminated due to hanging
echo "First build attempt terminated due to hanging. Trying to fix..."

# Check if we have the db checker script
if [ -f "check_swift_build_db.sh" ]; then
    echo "Running database checker..."
    ./check_swift_build_db.sh .
fi

# Try building again with clean artifacts
echo "Attempting selective clean build..."
$SWIFT_PATH package clean
$SWIFT_PATH build $BUILD_ARGS > build.log 2>&1 &
BUILD_PID=$!

if check_build_hanging $BUILD_PID; then
    echo "Build completed successfully after clean!"
    cat build.log
    rm build.log
    exit 0
fi

# Last resort, clean everything and rebuild
echo "Second build attempt also hung. Performing full clean..."
rm -rf .build
echo "Rebuilding from scratch: $SWIFT_PATH build $BUILD_ARGS"
$SWIFT_PATH build $BUILD_ARGS

echo "Build completed!"
rm -f build.log 