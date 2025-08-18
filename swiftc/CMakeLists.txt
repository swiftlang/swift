cmake_minimum_required(VERSION 3.20)
project(swiftc VERSION 1.0.0 LANGUAGES C CXX)

# Require C++20
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Handle ZSTD dependency - properly link with LLVM
find_library(ZSTD_LIBRARY NAMES zstd PATHS /usr/lib/x86_64-linux-gnu /usr/lib /lib)
if(ZSTD_LIBRARY)
    message(STATUS "Found ZSTD: ${ZSTD_LIBRARY}")
else()
    message(STATUS "ZSTD not found, will link with system zstd")
    set(ZSTD_LIBRARY "zstd")
endif()

# Create ZSTD target
add_library(zstd::libzstd_shared INTERFACE IMPORTED)
set_target_properties(zstd::libzstd_shared PROPERTIES
    INTERFACE_LINK_LIBRARIES "${ZSTD_LIBRARY}")

# Add ZSTD to the global link libraries
link_libraries(${ZSTD_LIBRARY})

# Find LLVM - try different versions
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Check LLVM version
if(LLVM_VERSION_MAJOR LESS 20)
    message(FATAL_ERROR "LLVM version must be at least 20, found ${LLVM_VERSION_MAJOR}")
endif()

# Include LLVM headers and definitions
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Use monolithic LLVM library for simpler linking
set(llvm_libs LLVM-20)

# Set include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-unused-parameter")

# Add subdirectories
add_subdirectory(lib)
add_subdirectory(tools)
add_subdirectory(stdlib)

# Add unit tests if GTest is available
find_package(GTest QUIET)
if(GTest_FOUND)
    message(STATUS "Found GTest, enabling unit tests")
    enable_testing()
    add_subdirectory(unittests)
else()
    message(WARNING "GTest not found, unit tests will not be built")
endif()