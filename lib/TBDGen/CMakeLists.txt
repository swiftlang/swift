add_subdirectory(tapi)

add_swift_library(swiftTBDGen STATIC
  ${TAPI_SOURCES}
  TBDGen.cpp
  LINK_LIBRARIES
    swiftAST
    swiftSIL
)
