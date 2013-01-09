add_subdirectory(SILGen)
add_swift_library(swiftSIL
  BasicBlock.cpp
  SIL.cpp
  SILBase.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILTypeInfo.cpp
  Instruction.cpp
  Verifier.cpp
  DEPENDS swiftAST)

