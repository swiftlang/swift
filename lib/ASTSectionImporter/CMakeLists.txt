add_swift_host_library(swiftASTSectionImporter STATIC
  ASTSectionImporter.cpp
  LLVM_COMPONENT_DEPENDS core)
target_link_libraries(swiftASTSectionImporter PRIVATE
  swiftBasic)

