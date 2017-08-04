add_swift_library(swiftParseSIL STATIC
  ParseSIL.cpp
  LINK_LIBRARIES
    swiftParse
    swiftSema
    swiftSIL
)

