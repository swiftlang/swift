add_swift_unittest(SwiftSyntaxTests
  DeclSyntaxTests.cpp
  ExprSyntaxTests.cpp
  GenericSyntaxTests.cpp
  RawSyntaxTests.cpp
  StmtSyntaxTests.cpp
  ThreadSafeCachingTests.cpp
  TypeSyntaxTests.cpp)

target_link_libraries(SwiftSyntaxTests
  swiftSyntax)
