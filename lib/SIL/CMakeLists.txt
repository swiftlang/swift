add_swift_library(swiftSIL STATIC
  AbstractionPattern.cpp
  Bridging.cpp
  Dominance.cpp
  DynamicCasts.cpp
  InstructionUtils.cpp
  Linker.cpp
  LoopInfo.cpp
  Mangle.cpp
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
  SILVTable.cpp
  SILWitnessTable.cpp
  TypeLowering.cpp
  LINK_LIBRARIES
    swiftSerialization
    swiftSema
)

# This property is only set by calls to clang_tablegen. It will not be set on
# standalone builds, so it can always be safely passed.
get_property(CLANG_TABLEGEN_TARGETS GLOBAL PROPERTY CLANG_TABLEGEN_TARGETS)
if(CLANG_TABLEGEN_TARGETS)
  add_dependencies(swiftSIL
                  ${CLANG_TABLEGEN_TARGETS})
endif(CLANG_TABLEGEN_TARGETS)
