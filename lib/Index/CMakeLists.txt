add_swift_host_library(swiftIndex STATIC
  Index.cpp
  IndexDataConsumer.cpp
  IndexRecord.cpp
  IndexSymbol.cpp)
target_link_libraries(swiftIndex PRIVATE
  swiftAST
  swiftIDE)
