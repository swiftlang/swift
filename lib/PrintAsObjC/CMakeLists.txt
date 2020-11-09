set_swift_llvm_is_available()
add_swift_host_library(swiftPrintAsObjC STATIC
  DeclAndTypePrinter.cpp
  ModuleContentsWriter.cpp
  PrintAsObjC.cpp)
target_link_libraries(swiftPrintAsObjC PRIVATE
  swiftAST
  swiftClangImporter
  swiftFrontend
  swiftIDE)

