add_swift_library(swiftRemoteAST STATIC
  RemoteAST.cpp
  LINK_LIBRARIES
    swiftSema swiftIRGen)
