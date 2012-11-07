add_subdirectory(SILGen)
add_swift_library(swiftSIL
  BasicBlock.cpp
  SIL.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  Instruction.cpp
  Verifier.cpp
  DEPENDS swiftAST)

