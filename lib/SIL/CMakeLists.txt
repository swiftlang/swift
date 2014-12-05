add_swift_library(swiftSIL
  Dominance.cpp
  DynamicCasts.cpp
  PrettyStackTrace.cpp
  SIL.cpp
  SILArgument.cpp
  SILBasicBlock.cpp
  SILBuilder.cpp
  SILFunction.cpp
  SILFunctionType.cpp
  SILGlobalVariable.cpp
  SILInstruction.cpp
  SILLocation.cpp
  SILMetadata.cpp
  SILModule.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILVTable.cpp
  SILWitnessTable.cpp
  TypeLowering.cpp
  Verifier.cpp
  SILType.cpp
  SILValue.cpp
  SILDeclRef.cpp
  LoopInfo.cpp
  Projection.cpp
  DEPENDS swiftAST swiftSerialization)

# Clang dependencies.
# FIXME: remove this.  SIL should not depend on Clang.
target_link_libraries(swiftSIL
  clangAnalysis
  clangAST
  clangLex
  clangBasic
  LLVMSupport)
