add_swift_executable(swift
  driver.cpp
  frontend_main.cpp
  DEPENDS swiftDriver swiftIRGen swiftSIL swiftSILGen swiftSILPasses swiftImmediate
          swiftSerialization swiftFrontend swiftClangImporter swiftIDE 
          swiftPrintAsObjC
  COMPONENT_DEPENDS bitreader bitwriter codegen ipo jit linker mcjit asmparser
                    selectiondag ${LLVM_TARGETS_TO_BUILD})

find_library(CORE_FOUNDATION NAMES CoreFoundation)

target_link_libraries(swift edit ${CORE_FOUNDATION})

if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}"
                   -DSWIFT_MODULE_CACHE_PATH="${SWIFT_MODULE_CACHE_PATH}" )
endif()

install(TARGETS swift
  RUNTIME DESTINATION bin)
