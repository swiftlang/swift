add_swift_host_library(swiftRemoteAST STATIC
  RemoteAST.cpp
  InProcessMemoryReader.cpp)
target_link_libraries(swiftRemoteAST PUBLIC
  swiftDemangling)
target_link_libraries(swiftRemoteAST PRIVATE
  swiftIRGen
  swiftSema)
