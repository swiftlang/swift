add_swift_library(swiftRemoteAST
  RemoteAST.cpp
  LINK_LIBRARIES
    swiftSema swiftIRGen)
