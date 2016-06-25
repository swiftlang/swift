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

# Include the current binary dir since we rely on gybed files.
include_directories(${CMAKE_CURRENT_BINARY_DIR})

add_dependencies(swiftClangImporter "${generated_include_targets}")
