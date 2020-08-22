add_swift_unittest(SwiftDriverTests
  FineGrainedDependencyGraphTests.cpp
  MockingFineGrainedDependencyGraphs.cpp
  TypeBodyFingerprintsDependencyGraphTests.cpp
  UnitTestSourceFileDepGraphFactory.cpp
)

target_link_libraries(SwiftDriverTests PRIVATE
   swiftAST
   swiftClangImporter
   swiftDriver)

target_compile_definitions(SwiftDriverTests PRIVATE
  SWIFTLIB_DIR=\"${SWIFTLIB_DIR}\")
