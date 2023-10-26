add_swift_unittest(SwiftSILOptimizerTests
  PartitionUtilsTest.cpp
)

target_link_libraries(SwiftSILOptimizerTests
   PRIVATE
   swiftSILOptimizer
   swiftIRGen
   swiftAST
   swiftFrontend
)
