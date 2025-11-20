add_swift_unittest(SwiftASTTests
  ArithmeticEvaluator.cpp
  ASTDumperTests.cpp
  ASTWalkerTests.cpp
  AvailabilityContextTests.cpp
  AvailabilityDomainTests.cpp
  IndexSubsetTests.cpp
  DiagnosticBehaviorTests.cpp
  DiagnosticConsumerTests.cpp
  DiagnosticGroupsTests.cpp
  DiagnosticFormattingTests.cpp
  DiagnosticInfoTests.cpp
  SourceLocTests.cpp
  TestContext.cpp
  TypeMatchTests.cpp
  VersionRangeTests.cpp
  ImportTests.cpp
)

target_link_libraries(SwiftASTTests
   PRIVATE
   swiftAST
   # FIXME: Circular dependencies.
   swiftParse
   swiftSema
)
