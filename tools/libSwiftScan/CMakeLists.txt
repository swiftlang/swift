include(SwiftWindowsSupport)
swift_swap_compiler_if_needed("libSwiftScan")

# Use an 'internal' name, this is primarily intended for SwiftDriver to import.
set(SWIFT_SCAN_LIB_NAME "_InternalSwiftScan")

set(LLVM_EXPORTED_SYMBOL_FILE
    ${CMAKE_CURRENT_SOURCE_DIR}/libSwiftScan.exports)

add_swift_host_library(libSwiftScan SHARED
  libSwiftScan.cpp
  c-include-check.c)

add_dependencies(libSwiftScan
  clang
  swiftDependencyScan)

target_link_libraries(libSwiftScan PRIVATE
    swiftDependencyScan)

set_target_properties(libSwiftScan
    PROPERTIES
    OUTPUT_NAME ${SWIFT_SCAN_LIB_NAME})

add_llvm_symbol_exports(libSwiftScan ${LLVM_EXPORTED_SYMBOL_FILE})

# Adds -dead_strip option
add_link_opts(libSwiftScan)
