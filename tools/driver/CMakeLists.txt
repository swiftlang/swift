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

install(TARGETS ${name}
  RUNTIME DESTINATION bin)

# Keep the name "swift_driver" around for a bit longer.
add_custom_command(TARGET swift POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E create_symlink swift swift_driver
  WORKING_DIRECTORY "${SWIFT_RUNTIME_OUTPUT_INTDIR}")

# Add a symlink to clang's includes so the built swift
# can find its resource dir when run in place.
add_custom_command(TARGET swift POST_BUILD
  COMMAND mkdir "-p" "${SWIFTLIB_DIR}/swift"
  COMMAND ln "-fns"
    "${SWIFT_PATH_TO_CLANG_BUILD}/${CMAKE_CFG_INTDIR}/lib/clang"
    "${SWIFTLIB_DIR}/swift/clang")
