add_swift_host_library(swiftSyntaxParse STATIC
  RawSyntaxTokenCache.cpp
  SyntaxTreeCreator.cpp
  LINK_LIBRARIES
    swiftParse
    swiftSyntax
)
