add_subdirectory(SILGen)
add_subdirectory(Passes)
add_swift_library(swiftSIL
  Dominance.cpp
  Mangle.cpp
  SIL.cpp
  SILBasicBlock.cpp
  SILBuilder.cpp
  SILInstruction.cpp
  SILFunction.cpp
  SILLocation.cpp
  SILModule.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILType.cpp
  TypeLowering.cpp
  Verifier.cpp
  DEPENDS swiftAST)

