add_swift_library(swiftSIL STATIC
  AbstractionPattern.cpp
  Bridging.cpp
  Dominance.cpp
  DynamicCasts.cpp
  InstructionUtils.cpp
  Linker.cpp
  LoopInfo.cpp
  PrettyStackTrace.cpp
  Projection.cpp
  SIL.cpp
  SILArgument.cpp
  SILBasicBlock.cpp
  SILBuilder.cpp
  SILCoverageMap.cpp
  SILDeclRef.cpp
  SILDefaultWitnessTable.cpp
  SILFunction.cpp
  SILFunctionType.cpp
  SILGlobalVariable.cpp
  SILInstruction.cpp
  SILInstructions.cpp
  SILLocation.cpp
  SILModule.cpp
  SILOpenedArchetypesTracker.cpp
  SILPrinter.cpp
  SILSuccessor.cpp
  SILType.cpp
  SILValue.cpp
  SILVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVTable.cpp
  SILWitnessTable.cpp
  TypeLowering.cpp
  ValueOwnershipKindClassifier.cpp
  LINK_LIBRARIES
    swiftSerialization
    swiftSema
)

# intrinsics_gen is the LLVM tablegen target that generates the include files
# where intrinsics and attributes are declared. See the comment in lib/AST for
# more detail.
if(NOT SWIFT_BUILT_STANDALONE)
  get_property(CLANG_TABLEGEN_TARGETS GLOBAL PROPERTY CLANG_TABLEGEN_TARGETS)
  add_dependencies(swiftSIL intrinsics_gen ${CLANG_TABLEGEN_TARGETS})
endif()
