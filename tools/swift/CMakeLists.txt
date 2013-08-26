add_swift_executable(swift
  Helpers.cpp
  Immediate.cpp
  SerializedDiagnosticConsumer.cpp
  swift.cpp
  DEPENDS swiftIRGen swiftParse swiftSema swiftClangImporter swiftAST swiftSIL
          swiftSILGen swiftSILPasses swiftSerialization swiftFrontend swiftIDE
  COMPONENT_DEPENDS bitreader bitwriter codegen ipo jit linker mcjit asmparser
                    irreader selectiondag ${LLVM_TARGETS_TO_BUILD})

find_library(CORE_FOUNDATION NAMES CoreFoundation)

target_link_libraries(swift edit ${CORE_FOUNDATION})

if(HAVE_DARWIN_MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}"
                   -DSWIFT_MODULE_CACHE_PATH="${SWIFT_MODULE_CACHE_PATH}" )
endif()

install(TARGETS swift
  RUNTIME DESTINATION bin)
