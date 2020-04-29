add_swift_unittest(SwiftDriverTests
  FineGrainedDependencyGraphTests.cpp
  MockingFineGrainedDependencyGraphs.cpp
  TypeBodyFingerprintsDependencyGraphTests.cpp
  UnitTestSourceFileDepGraphFactory.cpp
)

target_link_libraries(SwiftDriverTests
   PRIVATE
   swiftDriver
   swiftClangImporter
   swiftAST
)
