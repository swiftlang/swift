add_swift_unittest(SwiftFrontendTests
  FrontendToolTests.cpp
  ModuleLoadingTests.cpp)

target_link_libraries(SwiftFrontendTests
  PRIVATE
    swiftFrontend
    swiftFrontendTool)
