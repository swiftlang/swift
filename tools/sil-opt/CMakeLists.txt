add_swift_executable(sil-opt
  SILOpt.cpp
  DEPENDS swiftIRGen swiftParse swiftSema swiftAST swiftSIL
          swiftSILGen swiftSILPasses swiftSerialization swiftFrontend
  COMPONENT_DEPENDS option bitreader ${LLVM_TARGETS_TO_BUILD})

find_library(CORE_FOUNDATION NAMES CoreFoundation)

target_link_libraries(sil-opt edit ${CORE_FOUNDATION})

install(TARGETS sil-opt
  RUNTIME DESTINATION bin)
