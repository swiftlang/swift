cmake_minimum_required(VERSION 3.20)
project(swiftc VERSION 1.0.0 LANGUAGES C CXX)

# Require C++20
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Handle ZSTD dependency - use system library
find_library(ZSTD_LIBRARY NAMES zstd PATHS /opt/homebrew/lib /usr/local/lib /usr/lib)
if(ZSTD_LIBRARY)
    message(STATUS "Found ZSTD: ${ZSTD_LIBRARY}")
    add_library(zstd::libzstd_shared INTERFACE IMPORTED)
    set_target_properties(zstd::libzstd_shared PROPERTIES
        INTERFACE_LINK_LIBRARIES "${ZSTD_LIBRARY}")
else()
    message(WARNING "ZSTD not found, creating dummy target")
    add_library(zstd::libzstd_shared INTERFACE IMPORTED)
endif()

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
# Find LLVM components (complete set for ALL LLVM-supported platforms)
llvm_map_components_to_libnames(llvm_libs
    core
    support
    target
    mc
    asmprinter
    # Only include essential targets to avoid linking issues
    x86codegen
    x86asmparser
    asmparser
    # All LLVM target architectures (code generation)
    aarch64codegen
    amdgpucodegen
    armcodegen
    avrcodegen
    bpfcodegen
    hexagoncodegen
    lanaicodegen
    loongarchcodegen
    mipscodegen
    msp430codegen
    nvptxcodegen
    powerpccodegen
    riscvcodegen
    sparccodegen
    spirvcodegen
    systemzcodegen
    vecodegen
    webassemblycodegen
    xcorecodegen
    # Main ASM parsers
    aarch64asmparser
    amdgpuasmparser
    armasmparser
    asmparser
    avrasmparser
    bpfasmparser
    hexagonasmparser
    lanaiasmparser
    loongarchasmparser
    mipsasmparser
    msp430asmparser
    powerpcasmparser
    riscvasmparser
    sparcasmparser
    systemzasmparser
    veasmparser
    webassemblyasmparser
    x86asmparser
)

# Set include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Set compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -fno-rtti")

# Add subdirectories
add_subdirectory(lib)
add_subdirectory(tools)
add_subdirectory(stdlib)

