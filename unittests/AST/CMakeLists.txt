add_swift_unittest(SwiftASTTests
  DiagnosticConsumerTests.cpp
  SourceLocTests.cpp
  TestContext.cpp
  TypeMatchTests.cpp
  VersionRangeLattice.cpp
)

target_link_libraries(SwiftASTTests
   PRIVATE
   # FIXME: Circular dependencies.
   swiftParse
   swiftAST # Ordering matters, swiftAST needs to be after swiftParse
   swiftSema
   swiftSIL
)
