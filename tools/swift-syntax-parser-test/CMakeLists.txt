include(SwiftWindowsSupport)
swift_swap_compiler_if_needed("SwiftSyntaxParserTest")

add_swift_host_tool(swift-syntax-parser-test
  swift-syntax-parser-test.cpp
  LLVM_LINK_COMPONENTS
    Support
  SWIFT_COMPONENT tools
)
if(NOT SWIFT_BUILT_STANDALONE AND NOT CMAKE_C_COMPILER_ID MATCHES Clang)
  add_dependencies(swift-syntax-parser-test clang)
endif()
target_link_libraries(swift-syntax-parser-test
  PRIVATE
    libSwiftSyntaxParser
)

if(APPLE)
  # Prioritize finding the parser library from the build/lib directory.
  # Otherwise it may find it from the 'lib/swift/macosx' directory which could
  # be out-of-date.
  get_target_property(link_flags swift-syntax-parser-test LINK_FLAGS)
  set(link_flags "-Xlinker -rpath -Xlinker @executable_path/../lib ${link_flags}")
  set_property(TARGET swift-syntax-parser-test PROPERTY
      LINK_FLAGS "${link_flags}")
endif()

set_property(TARGET swift-syntax-parser-test APPEND_STRING PROPERTY
  COMPILE_FLAGS " -fblocks")
if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
  target_link_libraries(swift-syntax-parser-test PRIVATE
    BlocksRuntime)
endif()
