add_swift_library(swiftIRGen
  GenArray.cpp
  GenFunc.cpp
  GenOneOf.cpp
  GenTuple.cpp
  GenType.cpp
  IRGen.cpp
  IRGenFunction.cpp
  IRGenModule.cpp
  DEPENDS swiftAST)
