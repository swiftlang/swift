add_swift_host_library(swiftASTSectionImporter STATIC
  ASTSectionImporter.cpp
  LINK_LIBS swiftBasic
  LLVM_COMPONENT_DEPENDS core)

