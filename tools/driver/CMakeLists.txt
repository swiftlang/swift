add_swift_executable(swift
  driver.cpp
  api_notes.cpp
  frontend_main.cpp
  DEPENDS swiftDriver swiftIRGen swiftSIL swiftSILGen swiftSILPasses swiftImmediate
          swiftSerialization swiftFrontend swiftClangImporter swiftIDE 
          swiftPrintAsObjC swiftOption
  COMPONENT_DEPENDS bitreader bitwriter codegen ipo linker mcjit asmparser
                    selectiondag ${LLVM_TARGETS_TO_BUILD})

find_library(CORE_FOUNDATION NAMES CoreFoundation)

target_link_libraries(swift edit ${CORE_FOUNDATION})

# If building as part of clang, make sure the headers are installed.
if(SWIFT_PATH_TO_CLANG_BUILD STREQUAL ${CMAKE_BINARY_DIR})
  add_dependencies(swift clang-headers)
endif()

execute_process(
  COMMAND xcrun --toolchain default -f clang
  OUTPUT_VARIABLE default_toolchain_compiler)
get_filename_component(ARCLITE_BASE ${default_toolchain_compiler} PATH)
get_filename_component(ARCLITE_BASE ${ARCLITE_BASE} PATH)
set(SWIFT_REPL_ARCLITE "${ARCLITE_BASE}/lib/arc/libarclite_macosx.a")
if (SWIFT_REPL_ARCLITE)
  set_property(TARGET swift APPEND_STRING PROPERTY
    LINK_FLAGS " -lobjc -Wl,-force_load,\"${SWIFT_REPL_ARCLITE}\"")
endif (SWIFT_REPL_ARCLITE)

if(MODULES_SDK)
  add_definitions( -DSWIFT_MODULES_SDK="${MODULES_SDK}" )
endif()

install(TARGETS swift
  RUNTIME DESTINATION bin)

add_custom_command(TARGET swift POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E create_symlink swift swiftc
    WORKING_DIRECTORY ${SWIFT_RUNTIME_OUTPUT_INTDIR})

install(CODE "execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink swift swiftc WORKING_DIRECTORY ${CMAKE_INSTALL_PREFIX}/bin)")
