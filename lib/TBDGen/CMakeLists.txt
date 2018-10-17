add_subdirectory(tapi)

swift_add_library(swiftTBDGen STATIC
  ${TAPI_SOURCES}
  TBDGen.cpp
  LINK_LIBRARIES
    swiftAST
    swiftSIL
)
