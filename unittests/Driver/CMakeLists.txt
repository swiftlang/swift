add_swift_unittest(SwiftDriverTests
  CoarseGrainedDependencyGraphTests.cpp
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
