set(SWIFT_GYB_FLAGS
    "-DCFDatabaseFile=${SWIFT_SOURCE_DIR}/lib/ClangImporter/CFDatabase.def")

set(generated_include_sources SortedCFDatabase.def.gyb)

handle_gyb_sources(
    generated_include_targets
    generated_include_sources
    "")

add_swift_library(swiftClangImporter
  ClangDiagnosticConsumer.cpp
  ClangImporter.cpp
  ImportDecl.cpp
  ImportMacro.cpp
  ImportType.cpp
  LINK_LIBRARIES
    swiftAST
  COMPONENT_DEPENDS
    debuginfoDWARF
)


add_dependencies(swiftClangImporter "${generated_include_targets}")
