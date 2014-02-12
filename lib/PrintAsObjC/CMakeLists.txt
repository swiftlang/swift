add_swift_library(swiftPrintAsObjC
  PrintAsObjC.cpp
  DEPENDS swiftAST)

# Clang dependencies.
# FIXME: Clang should really export these in some reasonable manner.
target_link_libraries(swiftPrintAsObjC
  clangAST)
