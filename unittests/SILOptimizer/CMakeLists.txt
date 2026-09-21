add_swift_unittest(SwiftSILOptimizerTests
  IsolationHistory.cpp
  PartitionUtilsTest.cpp
)

target_link_libraries(SwiftSILOptimizerTests
   PRIVATE
   swiftSILOptimizer
   swiftIRGen
   swiftAST
   swiftFrontend
)
