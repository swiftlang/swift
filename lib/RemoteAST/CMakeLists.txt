add_swift_library(swiftRemoteAST STATIC
  RemoteAST.cpp
  InProcessMemoryReader.cpp
  LINK_LIBRARIES
    swiftSema swiftIRGen)
