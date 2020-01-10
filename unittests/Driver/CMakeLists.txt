add_swift_unittest(SwiftDriverTests
  CoarseGrainedDependencyGraphTests.cpp
)

target_link_libraries(SwiftDriverTests
   PRIVATE
   swiftDriver
)
