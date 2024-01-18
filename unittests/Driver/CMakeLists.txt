add_swift_unittest(SwiftDriverTests
  FineGrainedDependencyGraphTests.cpp
  MockingFineGrainedDependencyGraphs.cpp
  UnitTestSourceFileDepGraphFactory.cpp
)

target_link_libraries(SwiftDriverTests PRIVATE
   swiftAST
   swiftClangImporter
   swiftSema
   swiftDriver)
