# Cross-build `swift-frontend` to run hosted on a WebAssembly (Emscripten)
# runtime. The Swift half of the wasm-hosted toolchain; LLVM/clang uses
# EmscriptenHostLLVM.cmake.
# Consumed by the EmscriptenHostSwift build-script product, which also passes
# the path-dependent -D flags (LLVM_DIR, Clang_DIR, SDK/sysroot, native tools,
# and the EmscriptenHostSwiftLLDShim.cmake top-level include).
#
# These `set(... CACHE ...)` entries are no-FORCE: they apply only on a fresh
# build dir, not an existing one (even with --reconfigure). Remove the build
# dir and reconfigure after editing this file.

# BOOTSTRAPPING_MODE is BOOTSTRAPPING, but the product also passes
# SWIFT_NATIVE_SWIFT_TOOLS_PATH (a prebuilt native swiftc), which flips the
# effective mode to CROSSCOMPILE: that swiftc compiles SwiftCompilerSources
# instead of in-tree bootstrapping stages.
set(SWIFT_ENABLE_SWIFT_IN_SWIFT ON CACHE BOOL "")
set(SWIFT_BUILD_SWIFT_SYNTAX OFF CACHE BOOL "")
set(BOOTSTRAPPING_MODE "BOOTSTRAPPING" CACHE STRING "")

set(SWIFT_HOST_VARIANT_SDK "EMSCRIPTEN" CACHE STRING "")
set(SWIFT_HOST_VARIANT_ARCH "wasm32" CACHE STRING "")

# No immediate (JIT) mode on the wasm host.
set(SWIFT_INCLUDE_TOOLS ON CACHE BOOL "")
set(SWIFT_BUILD_IMMEDIATE_MODE OFF CACHE BOOL "")

# Auxiliary tool libraries not run on the wasm host.
set(SWIFT_TOOL_LIBSWIFTSCAN_BUILD OFF CACHE BOOL "")
set(SWIFT_TOOL_LIBSTATICMIRROR_BUILD OFF CACHE BOOL "")
set(SWIFT_TOOL_LIBMOCKPLUGIN_BUILD OFF CACHE BOOL "")

# The stdlib, overlays, and SourceKit are prebuilt inputs; this build produces
# the frontend, not the runtime.
set(SWIFT_BUILD_DYNAMIC_STDLIB OFF CACHE BOOL "")
set(SWIFT_BUILD_STATIC_STDLIB OFF CACHE BOOL "")
set(SWIFT_BUILD_REMOTE_MIRROR OFF CACHE BOOL "")
set(SWIFT_BUILD_SOURCEKIT OFF CACHE BOOL "")
set(SWIFT_BUILD_CLANG_OVERLAYS OFF CACHE BOOL "")
set(SWIFT_BUILD_DYNAMIC_SDK_OVERLAY OFF CACHE BOOL "")

set(SWIFT_INCLUDE_TESTS OFF CACHE BOOL "")
set(SWIFT_INCLUDE_DOCS OFF CACHE BOOL "")

# Emscripten link flags: grow memory up to 4 GiB (wasm32 max), access the host
# filesystem directly (NODERAWFS), and emit DWARF into a sidecar so the main
# module stays under the wasm engine's per-module size limit.
set(CMAKE_EXE_LINKER_FLAGS
    "-sALLOW_MEMORY_GROWTH=1 -sMAXIMUM_MEMORY=4294967296 -sSTACK_SIZE=8388608 -sINITIAL_MEMORY=134217728 -sNODERAWFS=1 -sENVIRONMENT=node -gseparate-dwarf"
    CACHE STRING "")

# Keep the build machine's path out of __FILE__ literals baked into the served frontend.
if(DEFINED ENV{TOOLCHAIN_FILE_PREFIX_MAP})
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -ffile-prefix-map=$ENV{TOOLCHAIN_FILE_PREFIX_MAP}" CACHE STRING "")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ffile-prefix-map=$ENV{TOOLCHAIN_FILE_PREFIX_MAP}" CACHE STRING "")
endif()
