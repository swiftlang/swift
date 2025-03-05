add_swift_unittest(SwiftFrontendTests
  ArgParsingTest.cpp
  FeatureParsingTest.cpp
  IsFeatureEnabledTests.cpp
  StrictConcurrencyTests.cpp)

target_link_libraries(SwiftFrontendTests
  PRIVATE
    swiftFrontend
    # FIXME: Circular dependencies (AST <-> Parse)
    swiftAST)
