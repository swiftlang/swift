set(LLVM_ENABLE_PROJECTS
    clang
    clang-tools-extra
    lld
    lldb
    CACHE STRING "")

set(LLVM_EXTERNAL_PROJECTS
    cmark
    swift
    CACHE STRING "")

set(LLVM_ENABLE_RUNTIMES
    compiler-rt
    CACHE STRING "")

# This forces libc++ to be built so disable it.
set(LLDB_INCLUDE_TESTS NO CACHE BOOL "")

# Compiler-RT configuration for macOS.
set(LLVM_BUILD_EXTERNAL_COMPILER_RT YES CACHE BOOL "Build Compiler-RT with just-built clang")
set(COMPILER_RT_ENABLE_IOS YES CACHE BOOL "Build iOS Compiler-RT libraries")

# NOTE(compnerd) always enable assertions, the toolchain will not provide enough
# context to resolve issues otherwise and may silently generate invalid output.
set(LLVM_ENABLE_ASSERTIONS YES CACHE BOOL "")

set(ENABLE_X86_RELAX_RELOCATIONS YES CACHE BOOL "")

# NOTE(compnerd) we can hardcode the default target triple since the cache files
# are target dependent.
set(LLVM_DEFAULT_TARGET_TRIPLE arm64-apple-darwin CACHE STRING "")

set(LLVM_APPEND_VC_REV NO CACHE BOOL "")
set(LLVM_ENABLE_PER_TARGET_RUNTIME_DIR YES CACHE BOOL "")
set(LLVM_ENABLE_PYTHON YES CACHE BOOL "")
set(CMAKE_MACOSX_RPATH YES CACHE BOOL "")

set(LLVM_TARGETS_TO_BUILD AArch64 ARM WebAssembly X86 CACHE STRING "")

# Disable certain targets to reduce the configure time or to avoid configuration
# differences (and in some cases weird build errors on a complete build).
set(LLVM_BUILD_LLVM_DYLIB NO CACHE BOOL "")
set(LLVM_BUILD_LLVM_C_DYLIB NO CACHE BOOL "")
set(LLVM_ENABLE_LIBEDIT NO CACHE BOOL "")
set(LLVM_ENABLE_LIBXML2 NO CACHE BOOL "")
set(LLVM_ENABLE_OCAMLDOC NO CACHE BOOL "")
set(LLVM_ENABLE_TERMINFO NO CACHE BOOL "")
set(LLVM_ENABLE_Z3_SOLVER NO CACHE BOOL "")
set(LLVM_ENABLE_ZLIB NO CACHE BOOL "")
set(LLVM_ENABLE_ZSTD NO CACHE BOOL "")
set(LLVM_INCLUDE_BENCHMARKS NO CACHE BOOL "")
set(LLVM_INCLUDE_DOCS NO CACHE BOOL "")
set(LLVM_INCLUDE_EXAMPLES NO CACHE BOOL "")
set(LLVM_INCLUDE_GO_TESTS NO CACHE BOOL "")
set(LLVM_TOOL_GOLD_BUILD NO CACHE BOOL "")
set(LLVM_TOOL_LLVM_SHLIB_BUILD NO CACHE BOOL "")

# Avoid swig dependency for lldb
set(LLDB_ALLOW_STATIC_BINDINGS YES CACHE BOOL "")
set(LLDB_USE_STATIC_BINDINGS YES CACHE BOOL "")
set(LLDB_ENABLE_PYTHON YES CACHE BOOL "")
set(LLDB_EMBED_PYTHON_HOME NO CACHE BOOL "")
set(LLDB_ENABLE_LIBXML2 NO CACHE BOOL "")

set(SWIFT_INCLUDE_DOCS YES CACHE BOOL "")
set(SWIFT_BUILD_ENABLE_PARSER_LIB YES CACHE BOOL "")
set(SWIFT_BUILD_STDLIB_EXTRA_TOOLCHAIN_CONTENT NO CACHE BOOL "")
set(SWIFT_BUILD_STDLIB_CXX_MODULE NO CACHE BOOL "")
set(SWIFT_BUILD_STATIC_STDLIB NO CACHE BOOL "")
set(SWIFT_BUILD_STATIC_SDK_OVERLAY NO CACHE BOOL "")

set(LLVM_INSTALL_BINUTILS_SYMLINKS YES CACHE BOOL "")
set(LLVM_INSTALL_TOOLCHAIN_ONLY YES CACHE BOOL "")
set(LLVM_TOOLCHAIN_TOOLS
    addr2line
    ar
    c++filt
    dsymutil
    dwp
    # lipo
    llvm-ar
    llvm-cov
    llvm-cvtres
    llvm-cxxfilt
    llvm-dlltool
    llvm-dwarfdump
    llvm-dwp
    llvm-lib
    llvm-lipo
    llvm-ml
    llvm-mt
    llvm-nm
    llvm-objcopy
    llvm-objdump
    llvm-pdbutil
    llvm-profdata
    llvm-ranlib
    llvm-rc
    llvm-readelf
    llvm-readobj
    llvm-size
    llvm-strings
    llvm-strip
    llvm-symbolizer
    llvm-undname
    nm
    objcopy
    objdump
    ranlib
    readelf
    size
    strings
    CACHE STRING "")

set(CLANG_TOOLS
    clang
    clangd
    clang-deps-launcher
    clang-features-file
    clang-format
    clang-resource-headers
    clang-scan-deps
    clang-tidy
    CACHE STRING "")

set(LLD_TOOLS
    lld
    CACHE STRING "")

set(LLDB_TOOLS
    liblldb
    lldb
    lldb-argdumper
    lldb-python-scripts
    lldb-server
    lldb-dap
    repl_swift
    CACHE STRING "")

set(SWIFT_INSTALL_COMPONENTS
    autolink-driver
    compiler
    clang-builtin-headers
    editor-integration
    tools
    sourcekit-inproc
    static-mirror-lib
    swift-remote-mirror
    swift-remote-mirror-headers
    swift-syntax-lib
    compiler-swift-syntax-lib
    CACHE STRING "")

set(LLVM_DISTRIBUTION_COMPONENTS
    IndexStore
    libclang
    libclang-headers
    LTO
    builtins
    runtimes
    ${LLVM_TOOLCHAIN_TOOLS}
    ${CLANG_TOOLS}
    ${LLD_TOOLS}
    ${LLDB_TOOLS}
    ${SWIFT_INSTALL_COMPONENTS}
    CACHE STRING "")
