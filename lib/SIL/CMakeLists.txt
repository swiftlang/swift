add_subdirectory(SILGen)
add_swift_library(swiftSIL
  Checker.cpp
  Dominance.cpp
  Mangle.cpp
  SILBasicBlock.cpp
  SILInstruction.cpp
  SILFunction.cpp
  SIL.cpp
  SILLocation.cpp
  SILModule.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILType.cpp
  TypeLowering.cpp
  Verifier.cpp
  DEPENDS swiftAST)

