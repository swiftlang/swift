

add_swift_host_library(swiftAPIDigester STATIC
  ModuleAnalyzerNodes.cpp
  ModuleDiagsConsumer.cpp)

target_link_libraries(swiftAPIDigester PRIVATE
  swiftIDE)

set_swift_llvm_is_available(swiftAPIDigester)
