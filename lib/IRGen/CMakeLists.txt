add_swift_library(swiftIRGen
  GenArray.cpp
  GenExpr.cpp
  GenFunc.cpp
  GenLocal.cpp
  GenOneOf.cpp
  GenStmt.cpp
  GenTuple.cpp
  GenType.cpp
  IRGen.cpp
  IRGenFunction.cpp
  IRGenModule.cpp
  DEPENDS swiftAST)
