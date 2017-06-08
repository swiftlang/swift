add_swift_unittest(SwiftASTTests
  SourceLocTests.cpp
  TestContext.cpp
  TypeMatchTests.cpp
  VersionRangeLattice.cpp
)

target_link_libraries(SwiftASTTests
   swiftAST
   # FIXME: Circular dependencies.
   swiftParse
   LLVMCoverage
)
