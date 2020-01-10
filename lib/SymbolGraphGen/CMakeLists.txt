add_swift_host_library(swiftSymbolGraphGen STATIC
  DeclarationFragmentPrinter.cpp
  Edge.cpp
  JSON.cpp
  Symbol.cpp
  SymbolGraph.cpp
  SymbolGraphGen.cpp
  SymbolGraphASTWalker.cpp)

target_link_libraries(swiftSymbolGraphGen
  swiftAST
  swiftFrontend
  swiftMarkup)
