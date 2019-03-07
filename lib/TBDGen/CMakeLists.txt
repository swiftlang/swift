add_subdirectory(tapi)

add_swift_host_library(swiftTBDGen STATIC
  ${TAPI_SOURCES}
  TBDGen.cpp
  LLVM_COMPONENT_DEPENDS
    demangle
)
target_link_libraries(swiftTBDGen PRIVATE
  swiftAST
  swiftSIL)
