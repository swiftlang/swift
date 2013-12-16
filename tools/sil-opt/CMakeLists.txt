add_swift_executable(sil-opt
  SILOpt.cpp
  DEPENDS swiftIRGen swiftSIL swiftSILGen swiftSILPasses swiftSerialization
          swiftFrontend swiftNullClangImporter
  COMPONENT_DEPENDS option bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

install(TARGETS sil-opt
  RUNTIME DESTINATION bin)
