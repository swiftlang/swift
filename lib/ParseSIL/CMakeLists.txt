add_swift_library(swiftParseSIL STATIC
  ParseSIL.cpp
  DEPENDS
    swift-syntax-generated-headers
  LINK_LIBRARIES
    swiftParse
    swiftSema
    swiftSIL
)

