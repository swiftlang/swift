add_swift_library(swiftSIL
  Dominance.cpp
  PrettyStackTrace.cpp
  SIL.cpp
  SILBasicBlock.cpp
  SILBuilder.cpp
  SILFunction.cpp
  SILInstruction.cpp
  SILLocation.cpp
  SILModule.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILType.cpp
  SILVTable.cpp
  TypeLowering.cpp
  Verifier.cpp
  DEPENDS swiftAST)

target_link_libraries(swiftSIL
  clangAST)

