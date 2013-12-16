add_swift_executable(sil-extract
  SILExtract.cpp
  DEPENDS swiftSIL swiftSILGen swiftSILPasses swiftSerialization
          swiftFrontend swiftNullClangImporter
  COMPONENT_DEPENDS option bitreader bitwriter support ${LLVM_TARGETS_TO_BUILD})

find_library(CORE_FOUNDATION NAMES CoreFoundation)

target_link_libraries(sil-extract edit ${CORE_FOUNDATION})

install(TARGETS sil-extract
  RUNTIME DESTINATION bin)
