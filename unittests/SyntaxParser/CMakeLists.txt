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

if(APPLE)
  # Prioritize finding the parser library from the build/lib directory.
  # Otherwise it may find it from the 'lib/swift/macosx' directory which could
  # be out-of-date.
  get_target_property(link_flags SwiftSyntaxParserTests LINK_FLAGS)
  set(link_flags "-Xlinker -rpath -Xlinker ${SWIFT_LIBRARY_OUTPUT_INTDIR} ${link_flags}")
  set_property(TARGET SwiftSyntaxParserTests PROPERTY
      LINK_FLAGS "${link_flags}")
endif()

set_property(TARGET SwiftSyntaxParserTests APPEND_STRING PROPERTY
  COMPILE_FLAGS " -fblocks")
if(SWIFT_NEED_EXPLICIT_LIBDISPATCH)
  target_link_libraries(SwiftSyntaxParserTests PRIVATE BlocksRuntime)
endif()
