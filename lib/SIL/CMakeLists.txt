if(SWIFT_ENABLE_TENSORFLOW)
  find_package(TensorFlow REQUIRED)
  include_directories(BEFORE "${TF_INCLUDE_DIR}" )
endif()

add_swift_host_library(swiftSIL STATIC
  AbstractionPattern.cpp
  BasicBlockUtils.cpp
  Bridging.cpp
  DebugUtils.cpp
  Dominance.cpp
  DynamicCasts.cpp
  GraphFunctionDeviceInfo.cpp
  InstructionUtils.cpp
  MemAccessUtils.cpp
  Linker.cpp
  LinearLifetimeChecker.cpp
  LoopInfo.cpp
  Notifications.cpp
  OperandOwnership.cpp
  OptimizationRemark.cpp
  OwnershipUtils.cpp
  PrettyStackTrace.cpp
  Projection.cpp
  SIL.cpp
  SILArgument.cpp
  SILBasicBlock.cpp
  SILBuilder.cpp
  SILConstants.cpp
  SILCoverageMap.cpp
  SILDebugScope.cpp
  SILDeclRef.cpp
  SILDefaultWitnessTable.cpp
  SILFunction.cpp
  SILFunctionType.cpp
  SILGlobalVariable.cpp
  SILInstruction.cpp
  SILInstructions.cpp
  SILLocation.cpp
  SILModule.cpp
  SILFunctionBuilder.cpp
  SILOpenedArchetypesTracker.cpp
  SILPrinter.cpp
  SILProfiler.cpp
  SILSuccessor.cpp
  SILType.cpp
  SILValue.cpp
  SILVerifier.cpp
  SILOwnershipVerifier.cpp
  SILVTable.cpp
  SILWitnessTable.cpp
  TypeLowering.cpp
  ValueOwnership.cpp
  LINK_LIBRARIES
    swiftSerialization
    swiftSema
  INTERFACE_LINK_LIBRARIES "${TF_LIBRARIES}"
)

# intrinsics_gen is the LLVM tablegen target that generates the include files
# where intrinsics and attributes are declared. swiftSIL depends on these
# headers.
# For more information see the comment at the top of lib/CMakeLists.txt.
add_dependencies(swiftSIL intrinsics_gen clang-tablegen-targets)
