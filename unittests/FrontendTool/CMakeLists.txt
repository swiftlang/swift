add_swift_unittest(SwiftFrontendToolTests
  FrontendToolTests.cpp
  ModuleLoadingTests.cpp)

target_link_libraries(SwiftFrontendToolTests
  PRIVATE
    swiftFrontend
    swiftFrontendTool)
