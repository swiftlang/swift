add_swift_unittest(SwiftFrontendTests
  FrontendToolTests.cpp
  FrontendObserverTests.cpp
  ModuleLoadingTests.cpp)

target_link_libraries(SwiftFrontendTests
  PRIVATE
    swiftFrontend
    swiftFrontendTool)
