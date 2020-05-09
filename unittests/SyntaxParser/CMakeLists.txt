include(SwiftWindowsSupport)
swift_swap_compiler_if_needed("SyntaxParserUnitTests")

add_swift_unittest(SwiftSyntaxParserTests
  SyntaxParserTests.cpp
)
if(NOT SWIFT_BUILT_STANDALONE AND NOT CMAKE_C_COMPILER_ID STREQUAL Clang)
  add_dependencies(SwiftSyntaxParserTests clang)
endif()

target_link_libraries(SwiftSyntaxParserTests
  PRIVATE
  libSwiftSyntaxParser)

if(CMAKE_SYSTEM_NAME STREQUAL Darwin)
  set_target_properties(SwiftSyntaxParserTests PROPERTIES
    BUILD_RPATH ${SWIFT_LIBRARY_OUTPUT_INTDIR})
endif()

set_property(TARGET SwiftSyntaxParserTests APPEND_STRING PROPERTY
  COMPILE_FLAGS " -fblocks")
if(NOT CMAKE_SYSTEM_NAME STREQUAL Darwin)
  target_link_libraries(SwiftSyntaxParserTests PRIVATE BlocksRuntime)
endif()
