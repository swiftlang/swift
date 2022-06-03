
add_swift_host_library(swiftPrintAsClang STATIC
  ClangSyntaxPrinter.cpp
  DeclAndTypePrinter.cpp
  ModuleContentsWriter.cpp
  PrimitiveTypeMapping.cpp
  PrintAsClang.cpp
  PrintClangFunction.cpp
  PrintClangValueType.cpp
  SwiftToClangInteropContext.cpp)
target_link_libraries(swiftPrintAsClang PRIVATE
  swiftAST
  swiftClangImporter
  swiftFrontend
  swiftIDE
  swiftIRGen)

set_swift_llvm_is_available(swiftPrintAsClang)
