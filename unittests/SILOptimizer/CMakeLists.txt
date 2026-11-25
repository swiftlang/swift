add_swift_unittest(SwiftSILOptimizerTests
  IsolationHistory.cpp
  PartitionUtilsTest.cpp
  SILDebugInfoExpressionTest.cpp
)

target_link_libraries(SwiftSILOptimizerTests
   PRIVATE
   swiftSILOptimizer
   swiftIRGen
   swiftAST
   swiftFrontend
)
