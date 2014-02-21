add_swift_executable(sil-dis
  SILDis.cpp
  DEPENDS swiftSIL swiftSerialization
          swiftFrontend swiftNullClangImporter
  COMPONENT_DEPENDS option bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

install(TARGETS sil-dis
  RUNTIME DESTINATION bin)
