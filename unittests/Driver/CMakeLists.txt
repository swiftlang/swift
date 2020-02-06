add_swift_unittest(SwiftDriverTests
  CoarseGrainedDependencyGraphTests.cpp
  ModuleDepGraphWithoutFingerprintsTests.cpp
  MockingFineGrainedDependencyGraphs.cpp
  ModuleDepGraphTests.cpp
)

target_link_libraries(SwiftDriverTests
   PRIVATE
   swiftDriver
   swiftClangImporter
   swiftAST
)
