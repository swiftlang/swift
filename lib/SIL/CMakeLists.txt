add_subdirectory(SILGen)
add_swift_library(swiftSIL
  BasicBlock.cpp
  Instruction.cpp
  SIL.cpp
  SILBase.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILTypeInfo.cpp
  TypeLowering.cpp
  Verifier.cpp
  DEPENDS swiftAST)

