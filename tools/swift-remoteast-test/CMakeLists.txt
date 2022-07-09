add_swift_host_tool(swift-remoteast-test
  swift-remoteast-test.cpp
  SWIFT_COMPONENT testsuite-tools
  THINLTO_LD64_ADD_FLTO_CODEGEN_ONLY
)
target_link_libraries(swift-remoteast-test
                      PRIVATE
                        swiftFrontendTool
                        swiftRemoteAST)
set_target_properties(swift-remoteast-test PROPERTIES ENABLE_EXPORTS 1)

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift-frontend clang-resource-headers)
endif()

