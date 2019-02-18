add_swift_host_tool(swift-syntax-parser-test
  swift-syntax-parser-test.cpp
  LLVM_COMPONENT_DEPENDS
    Support
  SWIFT_COMPONENT tools
)
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
if(SWIFT_NEED_EXPLICIT_LIBDISPATCH)
  target_link_libraries(swift-syntax-parser-test PRIVATE BlocksRuntime)
endif()
