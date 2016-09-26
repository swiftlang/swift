set(SWIFT_GYB_FLAGS
    "-DCFDatabaseFile=${SWIFT_SOURCE_DIR}/lib/ClangImporter/CFDatabase.def")

set(generated_include_sources SortedCFDatabase.def.gyb)

handle_gyb_sources(
    generated_include_targets
    generated_include_sources
    "")

add_swift_library(swiftClangImporter STATIC
  CFTypeInfo.cpp
  ClangAdapter.cpp
  ClangDiagnosticConsumer.cpp
  ClangImporter.cpp
  IAMInference.cpp
  ImportDecl.cpp
  ImportEnumInfo.cpp
  ImportMacro.cpp
  ImportName.cpp
  ImportType.cpp
  SwiftLookupTable.cpp
  LINK_LIBRARIES
    swiftAST
    swiftParse
)

# This property is only set by calls to clang_tablegen. It will not be set on
# standalone builds, so it can always be safely passed.
get_property(CLANG_TABLEGEN_TARGETS GLOBAL PROPERTY CLANG_TABLEGEN_TARGETS)
add_dependencies(swiftClangImporter
                "${generated_include_targets}"
                ${CLANG_TABLEGEN_TARGETS})
