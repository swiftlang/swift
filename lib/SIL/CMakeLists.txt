add_subdirectory(CFGGen)
add_swift_library(swiftSIL
  BasicBlock.cpp
  CFG.cpp
  CFGPrinter.cpp
  CFGSuccessor.cpp
  Instruction.cpp
  Verifier.cpp
  DEPENDS swiftAST)

