set(SWIFT_GYB_FLAGS
    "-DCFDatabaseFile=${SWIFT_SOURCE_DIR}/lib/ClangImporter/CFDatabase.def")

add_gyb_target(generated_sorted_cf_database
    SortedCFDatabase.def.gyb
    DEPENDS "${SWIFT_SOURCE_DIR}/lib/ClangImporter/CFDatabase.def")

add_swift_host_library(swiftClangImporter STATIC
  CFTypeInfo.cpp
  ClangAdapter.cpp
  ClangDerivedConformances.cpp
  ClangDiagnosticConsumer.cpp
  ClangImporter.cpp
  ClangImporterRequests.cpp
  ClangIncludePaths.cpp
  ClangModuleDependencyScanner.cpp
  ClangSourceBufferImporter.cpp
  SwiftDeclSynthesizer.cpp
  DWARFImporter.cpp
  ImportDecl.cpp
  ImportEnumInfo.cpp
  ImportMacro.cpp
  ImportName.cpp
  ImportType.cpp
  Serializability.cpp
  SwiftLookupTable.cpp
)
target_link_libraries(swiftClangImporter PRIVATE
  swiftAST
  swiftParse
  clangTooling
  LLVMBitstreamReader)

target_link_libraries(swiftClangImporter INTERFACE
  clangDependencyScanning)

# This property is only set by calls to clang_tablegen. It will not be set on
# standalone builds, so it can always be safely passed.
get_property(CLANG_TABLEGEN_TARGETS GLOBAL PROPERTY CLANG_TABLEGEN_TARGETS)
add_dependencies(swiftClangImporter
  generated_sorted_cf_database
  ${CLANG_TABLEGEN_TARGETS})

set_swift_llvm_is_available(swiftClangImporter)

# Mark - copy "cxx-interop.h" into the local include directory and install it
# into the compiler toolchain.
set(SWIFTINC_DIR
    "${CMAKE_BINARY_DIR}/${CMAKE_CFG_INTDIR}/include/swift")

add_custom_command(
    OUTPUT "${SWIFTINC_DIR}/cxx-interop.h"
    COMMAND "${CMAKE_COMMAND}" "-E" "copy" "${CMAKE_CURRENT_SOURCE_DIR}/cxx-interop.h" "${SWIFTINC_DIR}")

add_custom_target("copy_cxxInterop_support_header"
    DEPENDS "${SWIFTINC_DIR}/cxx-interop.h"
    COMMENT "Copying C++ interop support header to ${SWIFTINC_DIR}")

swift_install_in_component(FILES "${CMAKE_CURRENT_SOURCE_DIR}/cxx-interop.h"
                           TARGET ${SWIFT_CLANG_RESOURCE_DIR_SYMLINK_INSTALL_TARGET}
                           DESTINATION "include/swift"
                           COMPONENT compiler)

add_dependencies(swiftClangImporter
  "copy_cxxInterop_support_header")
