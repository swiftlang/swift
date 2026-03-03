

add_swift_host_library(swiftAPIDigester STATIC
  ModuleAnalyzerNodes.cpp
  ModuleDiagsConsumer.cpp)

target_link_libraries(swiftAPIDigester PRIVATE
  swiftIDE)

# Clang dependencies. These are private because APIDigester's public
# interface does not use Clang symbols.
target_link_libraries(swiftAPIDigester PRIVATE
  clangAST
  clangLex)

set_swift_llvm_is_available(swiftAPIDigester)
