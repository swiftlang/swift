add_swift_unittest(SwiftASTTests
  ArithmeticEvaluator.cpp
<<<<<<< HEAD
  # SWIFT_ENABLE_TENSORFLOW
=======
>>>>>>> swift-DEVELOPMENT-SNAPSHOT-2019-10-13-a
  IndexSubsetTests.cpp
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
)
