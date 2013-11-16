add_swift_library(swiftSIL
  Dominance.cpp
  PrettyStackTrace.cpp
  SIL.cpp
  SILBasicBlock.cpp
  SILBuilder.cpp
  SILFunction.cpp
  SILFunctionType.cpp
  SILGlobalVariable.cpp
  SILInstruction.cpp
  SILLocation.cpp
  SILModule.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILVTable.cpp
  TypeLowering.cpp
  Verifier.cpp
  DEPENDS swiftAST)

# Clang dependencies.
# FIXME: remove this.  SIL should not depend on Clang.
target_link_libraries(swiftSIL
  clangAST
  clangLex
  clangBasic
  LLVMSupport)
