add_swift_host_library(swiftSyntaxParse STATIC
  RawSyntaxTokenCache.cpp
  SyntaxTreeCreator.cpp)
target_link_libraries(swiftSyntaxParse PRIVATE
  swiftParse
  swiftSyntax)
