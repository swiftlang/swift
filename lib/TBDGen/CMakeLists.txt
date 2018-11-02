add_subdirectory(tapi)

add_swift_host_library(swiftTBDGen STATIC
  ${TAPI_SOURCES}
  TBDGen.cpp
  LINK_LIBS
    swiftAST
    swiftSIL
)
