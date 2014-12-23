add_swift_library(swiftClangImporter
  ClangDiagnosticConsumer.cpp
  ClangImporter.cpp
  ImportDecl.cpp
  ImportMacro.cpp
  ImportType.cpp
  LINK_LIBRARIES
    swiftAST)

