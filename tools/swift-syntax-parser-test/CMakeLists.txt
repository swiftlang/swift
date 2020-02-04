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

if(CMAKE_SYSTEM_NAME STREQUAL Darwin)
  set_target_properties(swift-syntax-parser-test PROPERTIES
    BUILD_WITH_INSTALL_RPATH YES
    INSTALL_RPATH @executable_path/../lib)
endif()

set_property(TARGET swift-syntax-parser-test APPEND_STRING PROPERTY
  COMPILE_FLAGS " -fblocks")
if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
  target_link_libraries(swift-syntax-parser-test PRIVATE
    BlocksRuntime)
endif()
