add_swift_executable(swift_old
  swift.cpp
  DEPENDS swiftIRGen swiftSIL swiftSILGen swiftSILPasses swiftSerialization
          swiftFrontend swiftClangImporter swiftIDE swiftImmediate
  COMPONENT_DEPENDS bitreader bitwriter codegen ipo jit linker mcjit asmparser
                    irreader selectiondag ${LLVM_TARGETS_TO_BUILD})

find_library(CORE_FOUNDATION NAMES CoreFoundation)

target_link_libraries(swift_old edit ${CORE_FOUNDATION})

if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}"
                   -DSWIFT_MODULE_CACHE_PATH="${SWIFT_MODULE_CACHE_PATH}" )
endif()

if (SWIFT_SUBMIT_VERSION)
  add_definitions(-DSWIFT_VERSION_STRING="swift-${SWIFT_SUBMIT_VERSION}.${SWIFT_SUBMIT_SUBVERSION}")
endif()

install(TARGETS ${name}
  RUNTIME DESTINATION bin)
