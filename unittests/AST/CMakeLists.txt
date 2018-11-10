add_swift_unittest(SwiftASTTests
  ArithmeticEvaluator.cpp
  DiagnosticConsumerTests.cpp
  # SWIFT_ENABLE_TENSORFLOW
  SILAutoDiffIndices.cpp
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
)
