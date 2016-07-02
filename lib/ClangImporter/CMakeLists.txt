set(SWIFT_GYB_FLAGS
    "-DCFDatabaseFile=${SWIFT_SOURCE_DIR}/lib/ClangImporter/CFDatabase.def")

set(generated_include_sources SortedCFDatabase.def.gyb)

handle_gyb_sources(
    generated_include_targets
    generated_include_sources
    "")

add_swift_library(swiftClangImporter STATIC
  CFTypeInfo.cpp
  ClangDiagnosticConsumer.cpp
  ClangImporter.cpp
  IAMInference.cpp
  ImportDecl.cpp
  ImportEnumInfo.cpp
  ImportMacro.cpp
  ImportType.cpp
  SwiftLookupTable.cpp
  LINK_LIBRARIES
    swiftAST
    swiftParse
)


add_dependencies(swiftClangImporter "${generated_include_targets}")
