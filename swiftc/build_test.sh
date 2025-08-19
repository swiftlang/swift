#!/bin/bash

echo "=== Building SwiftC Parser Coverage Test ==="

# Create build directory
mkdir -p build_test
cd build_test

# Configure with CMake
cat > CMakeLists.txt << 'EOF'
cmake_minimum_required(VERSION 3.16)
project(SwiftCTest)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Add compiler flags for better diagnostics
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")

# Find LLVM (optional, fallback if not found)
find_package(LLVM QUIET CONFIG)
if(LLVM_FOUND)
    message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
    include_directories(${LLVM_INCLUDE_DIRS})
    add_definitions(${LLVM_DEFINITIONS})
    llvm_map_components_to_libnames(llvm_libs support core)
else()
    message(STATUS "LLVM not found, building without LLVM support")
    set(llvm_libs "")
endif()

# Include directories
include_directories(../include)

# Source files
set(SOURCES
    ../test_coverage.cpp
    ../lib/Lexer/Lexer.cpp
    ../lib/Lexer/Token.cpp
    ../lib/Parser/Parser.cpp
    ../lib/Basic/Diagnostic.cpp
)

# Add the test executable
add_executable(test_coverage ${SOURCES})

# Link libraries
if(llvm_libs)
    target_link_libraries(test_coverage ${llvm_libs})
endif()
EOF

# Build
echo "Configuring..."
cmake . -DCMAKE_BUILD_TYPE=Debug

echo "Building..."
make -j4

if [ $? -eq 0 ]; then
    echo "✅ Build successful!"
    echo ""
    echo "=== Running Coverage Test ==="
    ./test_coverage
else
    echo "❌ Build failed!"
    exit 1
fi