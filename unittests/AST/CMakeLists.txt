add_swift_unittest(SwiftASTTests
  OverrideTests.cpp
  TestContext.cpp
  VersionRangeLattice.cpp
)

target_link_libraries(SwiftASTTests
   swiftAST
   # FIXME: Circular dependencies.
   swiftParse
)
