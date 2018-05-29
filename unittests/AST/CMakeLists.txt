add_swift_unittest(SwiftASTTests
  DiagnosticConsumerTests.cpp
  SourceLocTests.cpp
  TestContext.cpp
  TypeMatchTests.cpp
  VersionRangeLattice.cpp
)

target_link_libraries(SwiftASTTests
   PRIVATE
   swiftAST
   # FIXME: Circular dependencies.
   swiftParse
   swiftSema
   swiftSIL
)
