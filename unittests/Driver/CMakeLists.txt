add_swift_unittest(SwiftDriverTests
  DependencyGraphTests.cpp
)

target_link_libraries(SwiftDriverTests
   PRIVATE
   swiftDriver
)
