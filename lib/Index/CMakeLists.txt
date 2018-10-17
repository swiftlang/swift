swift_add_library(swiftIndex STATIC
  Index.cpp
  IndexDataConsumer.cpp
  IndexRecord.cpp
  IndexSymbol.cpp
  LINK_LIBRARIES
    swiftAST)
