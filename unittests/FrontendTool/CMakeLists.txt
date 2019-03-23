add_swift_unittest(SwiftFrontendTests
  ModuleLoadingTests.cpp)

target_link_libraries(SwiftFrontendTests
  PRIVATE
    swiftFrontend
    swiftFrontendTool)
