add_swift_host_library(swiftParseSIL STATIC
  ParseSIL.cpp
  LINK_LIBRARIES
    swiftParse
    swiftSema
    swiftSIL
)

