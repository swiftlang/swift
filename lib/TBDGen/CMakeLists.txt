add_subdirectory(tapi)

add_swift_host_library(swiftTBDGen STATIC
  ${TAPI_SOURCES}
  TBDGen.cpp
  LINK_LIBRARIES
    swiftAST
    swiftSIL
  LLVM_COMPONENT_DEPENDS
    demangle
)
