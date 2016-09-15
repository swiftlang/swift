add_swift_unittest(SwiftASTTests
  OverrideTests.cpp
  SourceLocTests.cpp
  TestContext.cpp
  VersionRangeLattice.cpp
)

target_link_libraries(SwiftASTTests
   swiftAST
   # FIXME: Circular dependencies.
   swiftParse
)
