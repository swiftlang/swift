#!/bin/bash
# swift-build-ubuntu-test.sh
# A test version of the wrapper script that simulates hanging builds

# Store the original arguments
BUILD_ARGS="$@"

# Create a dummy build.log for testing
echo "Building package..." > build.log
echo "Using Swift version 6.1..." >> build.log
echo "Resolving dependencies..." >> build.log

# Function to check if simulated build is hanging
check_build_hanging() {
    local pid=$1
    local start_time=$SECONDS
    
    # Check every 5 seconds
    sleep 5
    
    # Always report hanging for testing
    return 1
}

echo "Attempting standard build with arguments: $BUILD_ARGS"
sleep 1
BUILD_PID=$$

if check_build_hanging $BUILD_PID; then
    echo "Build completed successfully!"
    cat build.log
    rm build.log
    exit 0
fi

# Simulating a hanging build
echo "First build attempt terminated due to hanging. Trying to fix..."

# Check if we have the db checker script
if [ -f "check_swift_build_db.sh" ]; then
    echo "Running database checker..."
    ./check_swift_build_db.sh .
fi

# Second build attempt
echo "Attempting selective clean build..."
sleep 1
BUILD_PID=$$

if check_build_hanging $BUILD_PID; then
    echo "Build completed successfully after clean!"
    cat build.log
    rm build.log
    exit 0
fi

# Simulating another hanging build
echo "Second build attempt also hung. Performing full clean..."

# Check if .build directory exists and remove it
if [ -d ".build" ]; then
    echo "Removing .build directory..."
    # Don't actually remove, just pretend
    echo "Removed .build directory."
fi

echo "Rebuilding from scratch with arguments: $BUILD_ARGS"
echo "Adding new content to the build log..." >> build.log
echo "Completed rebuild successfully!" >> build.log
cat build.log

echo "Build completed!"
rm -f build.log 