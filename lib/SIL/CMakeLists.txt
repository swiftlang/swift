add_subdirectory(SILGen)
add_swift_library(swiftSIL
  SILBasicBlock.cpp
  SILInstruction.cpp
  SIL.cpp
  SILBase.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILType.cpp
  TypeLowering.cpp
  Verifier.cpp
  DEPENDS swiftAST)

