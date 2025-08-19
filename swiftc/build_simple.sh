#!/bin/bash

echo "=== Building Simple SwiftC Parser Test ==="

# Create build directory
mkdir -p build_simple
cd build_simple

# Configure with CMake (without LLVM)
cat > CMakeLists.txt << 'EOF'
cmake_minimum_required(VERSION 3.16)
project(SwiftCTest)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Add compiler flags for better diagnostics
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter")

# Include directories
include_directories(../include)

# Create a simple LLVM.h replacement
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/llvm_simple.h "
#pragma once
#include <string>
namespace swiftc {
using StringRef = const std::string&;
template<typename T> using ArrayRef = const std::vector<T>&;
template<typename T> using SmallVector = std::vector<T>;
template<typename T> using SmallVectorImpl = std::vector<T>;
template<typename T> using SmallString = std::string;
template<typename T, typename U> using DenseMap = std::map<T, U>;
template<typename T> using MutableArrayRef = std::vector<T>&;
template<typename T> T* cast(void* ptr) { return static_cast<T*>(ptr); }
template<typename T> const T* cast(const void* ptr) { return static_cast<const T*>(ptr); }
template<typename T> T* dyn_cast(void* ptr) { return dynamic_cast<T*>(ptr); }
template<typename T> const T* dyn_cast(const void* ptr) { return dynamic_cast<const T*>(ptr); }
template<typename T> bool isa(const void* ptr) { return dynamic_cast<const T*>(ptr) != nullptr; }
}
")

# Replace LLVM.h include
file(COPY ../include/swiftc/Basic/LLVM.h DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/swiftc/Basic/)
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/swiftc/Basic/LLVM.h "
#ifndef SWIFTC_BASIC_LLVM_H
#define SWIFTC_BASIC_LLVM_H
#include <string>
#include <vector>
#include <map>
namespace swiftc {
using StringRef = const std::string&;
template<typename T> using ArrayRef = const std::vector<T>&;
template<typename T> using SmallVector = std::vector<T>;
template<typename T> using SmallVectorImpl = std::vector<T>;
template<typename T> using SmallString = std::string;
template<typename T, typename U> using DenseMap = std::map<T, U>;
template<typename T> using MutableArrayRef = std::vector<T>&;
template<typename T> T* cast(void* ptr) { return static_cast<T*>(ptr); }
template<typename T> const T* cast(const void* ptr) { return static_cast<const T*>(ptr); }
template<typename T> T* dyn_cast(void* ptr) { return dynamic_cast<T*>(static_cast<T*>(ptr)); }
template<typename T> const T* dyn_cast(const void* ptr) { return dynamic_cast<const T*>(static_cast<const T*>(ptr)); }
template<typename T> bool isa(const void* ptr) { return dynamic_cast<const T*>(static_cast<const T*>(ptr)) != nullptr; }
}
#endif
")

include_directories(${CMAKE_CURRENT_BINARY_DIR})

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