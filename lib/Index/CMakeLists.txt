add_swift_library(swiftIndex
  Index.cpp
  IndexDataConsumer.cpp
  IndexSymbol.cpp
  LINK_LIBRARIES
    swiftAST)
