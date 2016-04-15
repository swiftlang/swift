add_swift_unittest(SwiftASTTests
  OverrideTests.cpp
  VersionRangeLattice.cpp
)

target_link_libraries(SwiftASTTests
   swiftAST
   # FIXME: Circular dependencies.
   swiftParse
)
