swift_add_library(swiftParseSIL STATIC
  ParseSIL.cpp
  LINK_LIBRARIES
    swiftParse
    swiftSema
    swiftSIL
)

