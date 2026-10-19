add_swift_unittest(SwiftFrontendTests
  ArgParsingTest.cpp
  CompileJobCacheKeyTest.cpp
  FeatureParsingTest.cpp
  IsFeatureEnabledTests.cpp
  StrictConcurrencyTests.cpp)

target_link_libraries(SwiftFrontendTests
  PRIVATE
    swiftFrontend
    LLVMTestingSupport
    # FIXME: Circular dependencies (AST <-> Parse)
    swiftAST)
