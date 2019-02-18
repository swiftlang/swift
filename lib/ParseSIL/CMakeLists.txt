add_swift_host_library(swiftParseSIL STATIC
  ParseSIL.cpp
  LINK_LIBRARIES
    swiftSyntaxParse
    swiftSema
    swiftSIL
)

