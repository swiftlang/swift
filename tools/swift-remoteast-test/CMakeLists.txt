add_swift_host_tool(swift-remoteast-test
  swift-remoteast-test.cpp
  SWIFT_COMPONENT tools
)
target_link_libraries(swift-remoteast-test
                      PRIVATE
                        swiftFrontendTool
                        swiftRemoteAST)

set_target_properties(swift-remoteast-test PROPERTIES ENABLE_EXPORTS 1)
if(HAVE_UNICODE_LIBEDIT)
  target_link_libraries(swift-remoteast-test PRIVATE edit)
endif()

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift clang-headers)
endif()

