add_swift_library(swiftIndex STATIC
  Index.cpp
  IndexDataConsumer.cpp
  IndexSymbol.cpp
  LINK_LIBS
    swiftAST)
