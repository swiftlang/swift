add_swift_unittest(SwiftASTTests
  ArithmeticEvaluator.cpp
  ASTDumperTests.cpp
  ASTWalkerTests.cpp
  IndexSubsetTests.cpp
  DiagnosticConsumerTests.cpp
  SourceLocTests.cpp
  TestContext.cpp
  TypeMatchTests.cpp
  VersionRangeLattice.cpp
  ImportTests.cpp
)

target_link_libraries(SwiftASTTests
   PRIVATE
   swiftAST
   # FIXME: Circular dependencies.
   swiftParse
   swiftSema
)
