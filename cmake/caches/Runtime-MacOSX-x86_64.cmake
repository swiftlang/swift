set(SWIFT_HOST_VARIANT_SDK OSX CACHE STRING "")
set(SWIFT_HOST_VARIANT_ARCH x86_64 CACHE STRING "")
set(SWIFT_DARWIN_SUPPORTED_ARCHS x86_64 CACHE STRING "")
set(SWIFT_PRIMARY_VARIANT_ARCH x86_64 CACHE STRING "")

# NOTE(compnerd) disable the tools, we are trying to build just the standard
# library.
set(SWIFT_INCLUDE_TOOLS NO CACHE BOOL "")

# NOTE(compnerd) cannot build tests since the tests require the toolchain
set(SWIFT_INCLUDE_TESTS NO CACHE BOOL "")

# NOTE(compnerd) cannot build docs since that requires perl
set(SWIFT_INCLUDE_DOCS NO CACHE BOOL "")

# NOTE(compnerd) these are part of the toolchain, not the runtime.
set(SWIFT_BUILD_SOURCEKIT NO CACHE BOOL "")

# NOTE(compnerd) build with the compiler specified, not a just built compiler.
set(SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER YES CACHE BOOL "")
