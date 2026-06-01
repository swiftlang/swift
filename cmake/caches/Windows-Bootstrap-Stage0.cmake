include("${CMAKE_CURRENT_LIST_DIR}/Windows-Bootstrap-Core.cmake")

set(LLVM_ENABLE_PROJECTS
      clang
      lld
    CACHE STRING "")

set(LLVM_TOOLCHAIN_TOOLS "" CACHE STRING "")

set(LLD_TOOLS
      lld
    CACHE STRING "")

set(SWIFT_INSTALL_COMPONENTS
      compiler
      compiler-swift-syntax-lib
      swift-syntax-lib
      clang-builtin-headers
    CACHE STRING "")

set(LLVM_DISTRIBUTION_COMPONENTS
      ${LLVM_TOOLCHAIN_TOOLS}
      ${CLANG_TOOLS}
      ${LLD_TOOLS}
      ${SWIFT_INSTALL_COMPONENTS}
    CACHE STRING "")
