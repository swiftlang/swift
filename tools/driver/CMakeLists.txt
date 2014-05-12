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

get_filename_component(ARCLITE_BASE ${CMAKE_CXX_COMPILER} PATH)
get_filename_component(ARCLITE_BASE ${ARCLITE_BASE} PATH)
set(SWIFT_REPL_ARCLITE "${ARCLITE_BASE}/lib/arc/libarclite_macosx.a" 
    CACHE FILEPATH "Path to arclite to be linked into the compiler for the REPL")
if (SWIFT_REPL_ARCLITE)
  set_property(TARGET swift APPEND_STRING PROPERTY
    LINK_FLAGS "-lobjc -Wl,-force_load,${SWIFT_REPL_ARCLITE}")
endif (SWIFT_REPL_ARCLITE)

if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

install(TARGETS swift
  RUNTIME DESTINATION bin)
