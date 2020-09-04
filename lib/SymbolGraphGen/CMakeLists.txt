add_swift_host_library(swiftSymbolGraphGen STATIC
  AvailabilityMixin.cpp
  DeclarationFragmentPrinter.cpp
  Edge.cpp
  JSON.cpp
  Symbol.cpp
  SymbolGraph.cpp
  SymbolGraphGen.cpp
  SymbolGraphASTWalker.cpp)

target_link_libraries(swiftSymbolGraphGen PRIVATE
  swiftAST
  swiftFrontend
  swiftMarkup)
