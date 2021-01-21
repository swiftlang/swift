add_swift_unittest(SwiftSyntaxTests
  DeclSyntaxTests.cpp
  ExprSyntaxTests.cpp
  GenericSyntaxTests.cpp
  StmtSyntaxTests.cpp
  SyntaxCollectionTests.cpp
  TriviaTests.cpp
  TypeSyntaxTests.cpp
  UnknownSyntaxTests.cpp
  )

target_link_libraries(SwiftSyntaxTests
  PRIVATE
  swiftSyntax)
