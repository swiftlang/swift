
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
