add_swift_unittest(SwiftDriverTests
  CoarseGrainedDependencyGraphTests.cpp
  FineGrainedDependencyGraphTests.cpp
  TypeBodyFingerprintsDependencyGraphTests.cpp
)

target_link_libraries(SwiftDriverTests
   PRIVATE
   swiftDriver
   swiftClangImporter
   swiftAST
)
