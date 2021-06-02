# In the absence of fine grained tablegen dependencies we need to ensure that
# Swift's libraries all build after the LLVM & Clang tablegen-generated headers
# are generated. When building out-of-tree (as with build-script) LLVM & Clang's
# CMake configuration files create these targets as dummies so we can safely
# depend on them directly here (See: SR-6026)
# LLVM_COMMON_DEPENDS is a construct from the LLVM build system. It is a special
# purpose variable that provides common dependencies for all libraries, and
# executables generated when it is set. CMake's scoping rules enforce that these
# new dependencies will only be added to targets created under Swift's lib
# directory.
list(APPEND LLVM_COMMON_DEPENDS intrinsics_gen clang-tablegen-targets)

# Add generated libSyntax headers to global dependencies.
list(APPEND LLVM_COMMON_DEPENDS swift-syntax-generated-headers)
list(APPEND LLVM_COMMON_DEPENDS swift-parse-syntax-generated-headers)

add_swift_host_library(TestCPPLib
  STATIC
  CPPLib.cpp)

add_swift_host_library(TestPureSwiftSharedLib
  SHARED
  PURE_SWIFT
  Klass.swift)
target_link_libraries(TestPureSwiftSharedLib PRIVATE TestCPPLib)
target_link_options(TestPureSwiftSharedLib PRIVATE "SHELL:-import-objc-header ${CMAKE_CURRENT_SOURCE_DIR}/CPPLib.h")

add_swift_host_library(TestPureSwiftStaticLib
  STATIC
  PURE_SWIFT
  Klass.swift)
target_link_libraries(TestPureSwiftStaticLib PRIVATE TestCPPLib)
target_compile_options(TestPureSwiftStaticLib PRIVATE "SHELL:-import-objc-header ${CMAKE_CURRENT_SOURCE_DIR}/CPPLib.h")

add_swift_host_tool(TestCPPToolLinkSwiftSharedLib
  CPPTool.cpp
  SWIFT_COMPONENT testsuite-tools
  )
target_link_libraries(TestCPPToolLinkSwiftSharedLib PRIVATE TestPureSwiftSharedLib)

add_swift_host_tool(TestCPPToolLinkSwiftStaticLib
  CPPTool.cpp
  SWIFT_COMPONENT testsuite-tools
  )
target_link_libraries(TestCPPToolLinkSwiftStaticLib
  PRIVATE
  TestPureSwiftStaticLib)
