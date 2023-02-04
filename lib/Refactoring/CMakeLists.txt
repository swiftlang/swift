add_swift_host_library(swiftRefactoring STATIC
  Refactoring.cpp)

target_link_libraries(swiftRefactoring PRIVATE
  swiftAST
  swiftClangImporter
  swiftIDE
  swiftIndex
  swiftParse
  swiftSema)

set_swift_llvm_is_available(swiftRefactoring)

