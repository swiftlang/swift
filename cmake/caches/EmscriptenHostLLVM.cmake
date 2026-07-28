# Cross-build LLVM to run hosted on a WebAssembly (Emscripten) runtime. Consumed
# by the EmscriptenHostLLVM build-script product via `emcmake cmake -C ...`.
# Standalone:
#   emcmake cmake -G Ninja -S <llvm-project>/llvm -B <build> \
#     -C <swift>/cmake/caches/EmscriptenHostLLVM.cmake -DLLVM_TABLEGEN=<native llvm-tblgen>
#
# Pin the host/default triples explicitly: auto-detection runs config.guess on
# the build machine and bakes in the build host, and the default target triple
# does not reliably inherit the host triple.

set(LLVM_HOST_TRIPLE "wasm32-unknown-emscripten" CACHE STRING "")
set(LLVM_DEFAULT_TARGET_TRIPLE "wasm32-unknown-emscripten" CACHE STRING "")
set(LLVM_TARGETS_TO_BUILD "WebAssembly" CACHE STRING "")
set(LLVM_ENABLE_PROJECTS "clang;lld" CACHE STRING "")

set(LLVM_TOOL_LLVM_DRIVER_BUILD ON CACHE BOOL "")

set(LLVM_DISTRIBUTION_COMPONENTS "clang;lld" CACHE STRING "")

set(CLANG_SPAWN_CC1 OFF CACHE BOOL "")

set(LLVM_BUILD_LLVM_DYLIB OFF CACHE BOOL "")
set(LLVM_LINK_LLVM_DYLIB OFF CACHE BOOL "")
set(CLANG_LINK_CLANG_DYLIB OFF CACHE BOOL "")

set(CLANG_ENABLE_ARCMT OFF CACHE BOOL "")
set(CLANG_ENABLE_STATIC_ANALYZER OFF CACHE BOOL "")

set(LLVM_llvm-driver_LINKER_FLAGS
    "-sALLOW_MEMORY_GROWTH=1;-sSTACK_SIZE=8388608;-sINITIAL_MEMORY=134217728;-sMODULARIZE=1;-sEXPORT_NAME=createLLVMDriver;-sFORCE_FILESYSTEM=1;-sEXIT_RUNTIME=0;-sENVIRONMENT=node,web;-sEXPORTED_RUNTIME_METHODS=FS,callMain;-gseparate-dwarf"
    CACHE STRING "")

set(LLVM_ENABLE_THREADS OFF CACHE BOOL "")
set(LLVM_ENABLE_PLUGINS OFF CACHE BOOL "")
set(LLVM_ENABLE_ZLIB OFF CACHE BOOL "")
set(LLVM_ENABLE_ZSTD OFF CACHE BOOL "")
set(LLVM_ENABLE_LIBXML2 OFF CACHE BOOL "")
set(LLVM_ENABLE_LIBEDIT OFF CACHE BOOL "")

set(LLVM_INCLUDE_TESTS OFF CACHE BOOL "")
set(LLVM_INCLUDE_BENCHMARKS OFF CACHE BOOL "")
set(LLVM_INCLUDE_EXAMPLES OFF CACHE BOOL "")
