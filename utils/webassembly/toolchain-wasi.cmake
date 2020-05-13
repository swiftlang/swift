set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_VERSION 1)
set(CMAKE_SYSTEM_PROCESSOR wasm32)
set(triple wasm32-unknown-wasi)

set(WASI_SDK_PREFIX "${SWIFT_SOURCE_PREFIX}/wasi-sdk")

set(CMAKE_C_COMPILER ${WASI_SDK_PREFIX}/bin/clang)
set(CMAKE_CXX_COMPILER ${WASI_SDK_PREFIX}/bin/clang++)
set(CMAKE_AR ${WASI_SDK_PREFIX}/bin/llvm-ar CACHE STRING "wasi-sdk build")
set(CMAKE_RANLIB ${WASI_SDK_PREFIX}/bin/llvm-ranlib CACHE STRING "wasi-sdk build")
set(CMAKE_C_COMPILER_TARGET ${triple} CACHE STRING "wasi-sdk build")
set(CMAKE_CXX_COMPILER_TARGET ${triple} CACHE STRING "wasi-sdk build")
set(CMAKE_EXE_LINKER_FLAGS "-Wl,--no-threads" CACHE STRING "wasi-sdk build")

set(CMAKE_SYSROOT ${WASI_SDK_PREFIX}/share/wasi-sysroot CACHE STRING "wasi-sdk build")
set(CMAKE_STAGING_PREFIX ${WASI_SDK_PREFIX}/share/wasi-sysroot CACHE STRING "wasi-sdk build")

# Don't look in the sysroot for executables to run during the build
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
# Only look in the sysroot (not in the host paths) for the rest
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE NEVER)

# Some other hacks
set(CMAKE_C_COMPILER_WORKS ON)
set(CMAKE_CXX_COMPILER_WORKS ON)
