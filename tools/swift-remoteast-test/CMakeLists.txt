add_swift_host_tool(swift-remoteast-test
  swift-remoteast-test.cpp
  LINK_LIBRARIES
    swiftFrontendTool
    swiftRemoteAST
  SWIFT_COMPONENT tools
)

set_target_properties(swift-remoteast-test PROPERTIES ENABLE_EXPORTS 1)
if(HAVE_UNICODE_LIBEDIT)
  target_link_libraries(swift-remoteast-test edit)
endif()

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift clang-headers)
endif()

