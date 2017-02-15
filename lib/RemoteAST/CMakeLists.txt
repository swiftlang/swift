add_swift_library(swiftRemoteAST STATIC
  RemoteAST.cpp
  InProcessMemoryReader.cpp
  LINK_LIBS
    swiftSema swiftIRGen)
