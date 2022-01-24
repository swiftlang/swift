
add_swift_host_library(swiftPrintAsClang STATIC
  DeclAndTypePrinter.cpp
  ModuleContentsWriter.cpp
  PrintAsClang.cpp)
target_link_libraries(swiftPrintAsClang PRIVATE
  swiftAST
  swiftClangImporter
  swiftFrontend
  swiftIDE)

set_swift_llvm_is_available(swiftPrintAsClang)
