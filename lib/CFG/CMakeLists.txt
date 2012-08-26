add_swift_library(swiftCFG
  BasicBlock.cpp
  CFG.cpp
  CFGPrinter.cpp
  CFGSuccessor.cpp
  Instruction.cpp
  Verifier.cpp
  DEPENDS swiftAST)

