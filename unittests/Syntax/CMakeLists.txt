add_swift_unittest(SwiftSyntaxTests
  DeclSyntaxTests.cpp
  GenericSyntaxTests.cpp
  RawSyntaxTests.cpp
  StmtSyntaxTests.cpp
  TypeSyntaxTests.cpp)

target_link_libraries(SwiftSyntaxTests
  swiftSyntax)
