include(SwiftWindowsSupport)
swift_swap_compiler_if_needed("libSwiftScan")

# Use an 'internal' name, this is primarily intended for SwiftDriver to import.
set(SWIFT_SCAN_LIB_NAME "_InternalSwiftScan")

set(LLVM_EXPORTED_SYMBOL_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/libSwiftScan.exports)

add_swift_host_library(libSwiftScan SHARED
  libSwiftScan.cpp
  c-include-check.c
  HAS_LIBSWIFT)

add_dependencies(libSwiftScan
  clang
  swiftDependencyScan)

target_link_libraries(libSwiftScan PRIVATE
    swiftDependencyScan
    swiftDriverTool
    libswift)

set_target_properties(libSwiftScan
    PROPERTIES
    OUTPUT_NAME ${SWIFT_SCAN_LIB_NAME})

add_llvm_symbol_exports(libSwiftScan ${LLVM_EXPORTED_SYMBOL_FILE})

# Adds -dead_strip option
add_link_opts(libSwiftScan)

add_dependencies(compiler libSwiftScan)
swift_install_in_component(TARGETS libSwiftScan
  ARCHIVE DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}" COMPONENT compiler
  LIBRARY DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_LIB_SUBDIR}" COMPONENT compiler
  RUNTIME DESTINATION "bin" COMPONENT compiler)
swift_install_in_component(DIRECTORY "${SWIFT_MAIN_INCLUDE_DIR}/swift-c/DependencyScan/"
                           DESTINATION "lib${LLVM_LIBDIR_SUFFIX}/swift/${SWIFT_SCAN_LIB_NAME}"
                           COMPONENT compiler)
