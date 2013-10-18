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

if (SWIFT_SUBMIT_VERSION)
  add_definitions(-DSWIFT_VERSION_STRING="swift-${SWIFT_SUBMIT_VERSION}.${SWIFT_SUBMIT_SUBVERSION}")
endif()

# Add a run-path so that the interpreter can find the swift runtime libraries.
if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set_target_properties(swift
    PROPERTIES
    LINK_FLAGS "-Wl,-rpath -Wl,@executable_path/../lib/swift")
endif()

install(TARGETS swift
  RUNTIME DESTINATION bin)
