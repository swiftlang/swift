add_swift_executable(swift-remoteast-test
  swift-remoteast-test.cpp
  LINK_LIBRARIES
    swiftFrontendTool
    swiftRemoteAST)

target_link_libraries(swift-remoteast-test edit)

# If building as part of clang, make sure the headers are installed.
if(NOT SWIFT_BUILT_STANDALONE)
  add_dependencies(swift clang-headers)
endif()

