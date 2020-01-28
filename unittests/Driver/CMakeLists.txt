add_swift_unittest(SwiftDriverTests
  CoarseGrainedDependencyGraphTests.cpp
  FineGrainedDependencyGraphTests.cpp
)

target_link_libraries(SwiftDriverTests
   PRIVATE
   swiftDriver
   swiftClangImporter
   swiftAST
)
