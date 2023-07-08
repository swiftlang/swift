add_pure_swift_host_library(swiftSILOptimizerSwift STATIC
  Sources/AST/DiagnosticEngine.swift

  Sources/Basic/SourceLoc.swift
  Sources/Basic/Utils.swift

  Sources/Optimizer/Analysis/AliasAnalysis.swift
  Sources/Optimizer/Analysis/CalleeAnalysis.swift
  Sources/Optimizer/Analysis/DeadEndBlocksAnalysis.swift
  Sources/Optimizer/Analysis/DominatorTree.swift
  Sources/Optimizer/Analysis/PostDominatorTree.swift

  Sources/Optimizer/DataStructures/BasicBlockRange.swift
  Sources/Optimizer/DataStructures/DeadEndBlocks.swift
  Sources/Optimizer/DataStructures/FunctionUses.swift
  Sources/Optimizer/DataStructures/InstructionRange.swift
  Sources/Optimizer/DataStructures/ReachableBlocks.swift
  Sources/Optimizer/DataStructures/Set.swift
  Sources/Optimizer/DataStructures/Stack.swift
  Sources/Optimizer/DataStructures/Worklist.swift

  Sources/Optimizer/FunctionPasses/AssumeSingleThreaded.swift
  Sources/Optimizer/FunctionPasses/CleanupDebugSteps.swift
  Sources/Optimizer/FunctionPasses/ComputeEscapeEffects.swift
  Sources/Optimizer/FunctionPasses/ComputeSideEffects.swift
  Sources/Optimizer/FunctionPasses/DeadStoreElimination.swift
  Sources/Optimizer/FunctionPasses/InitializeStaticGlobals.swift
  Sources/Optimizer/FunctionPasses/ObjectOutliner.swift
  Sources/Optimizer/FunctionPasses/ObjCBridgingOptimization.swift
  Sources/Optimizer/FunctionPasses/MergeCondFails.swift
  Sources/Optimizer/FunctionPasses/NamedReturnValueOptimization.swift
  Sources/Optimizer/FunctionPasses/ReleaseDevirtualizer.swift
  Sources/Optimizer/FunctionPasses/SimplificationPasses.swift
  Sources/Optimizer/FunctionPasses/StackPromotion.swift
  Sources/Optimizer/FunctionPasses/StripObjectHeaders.swift

  Sources/Optimizer/InstructionSimplification/SimplifyApply.swift
  Sources/Optimizer/InstructionSimplification/SimplifyBeginCOWMutation.swift
  Sources/Optimizer/InstructionSimplification/SimplifyBranch.swift
  Sources/Optimizer/InstructionSimplification/SimplifyBuiltin.swift
  Sources/Optimizer/InstructionSimplification/SimplifyCondBranch.swift
  Sources/Optimizer/InstructionSimplification/SimplifyCondFail.swift
  Sources/Optimizer/InstructionSimplification/SimplifyDebugStep.swift
  Sources/Optimizer/InstructionSimplification/SimplifyDestructure.swift
  Sources/Optimizer/InstructionSimplification/SimplifyGlobalValue.swift
  Sources/Optimizer/InstructionSimplification/SimplifyInitEnumDataAddr.swift
  Sources/Optimizer/InstructionSimplification/SimplifyLoad.swift
  Sources/Optimizer/InstructionSimplification/SimplifyPartialApply.swift
  Sources/Optimizer/InstructionSimplification/SimplifyRefCasts.swift
  Sources/Optimizer/InstructionSimplification/SimplifyRetainReleaseValue.swift
  Sources/Optimizer/InstructionSimplification/SimplifyStrongRetainRelease.swift
  Sources/Optimizer/InstructionSimplification/SimplifyStructExtract.swift
  Sources/Optimizer/InstructionSimplification/SimplifyTupleExtract.swift
  Sources/Optimizer/InstructionSimplification/SimplifyUncheckedEnumData.swift
  Sources/Optimizer/InstructionSimplification/SimplifyValueToBridgeObject.swift

  Sources/Optimizer/ModulePasses/MandatoryPerformanceOptimizations.swift
  Sources/Optimizer/ModulePasses/ReadOnlyGlobalVariables.swift
  Sources/Optimizer/ModulePasses/StackProtection.swift

  Sources/Optimizer/PassManager/Context.swift
  Sources/Optimizer/PassManager/ModulePassContext.swift
  Sources/Optimizer/PassManager/Options.swift
  Sources/Optimizer/PassManager/Passes.swift
  Sources/Optimizer/PassManager/PassRegistration.swift

  Sources/Optimizer/TestPasses/FunctionUsesDumper.swift
  Sources/Optimizer/TestPasses/AccessDumper.swift
  Sources/Optimizer/TestPasses/DeadEndBlockDumper.swift
  Sources/Optimizer/TestPasses/EscapeInfoDumper.swift
  Sources/Optimizer/TestPasses/MemBehaviorDumper.swift
  Sources/Optimizer/TestPasses/SILPrinter.swift
  Sources/Optimizer/TestPasses/RangeDumper.swift
  Sources/Optimizer/TestPasses/RunUnitTests.swift
  Sources/Optimizer/TestPasses/TestInstructionIteration.swift

  Sources/Optimizer/Utilities/EscapeUtils.swift
  Sources/Optimizer/Utilities/OptUtils.swift
  Sources/Optimizer/Utilities/WalkUtils.swift
  Sources/Optimizer/Utilities/AccessUtils.swift
  Sources/Optimizer/Utilities/StaticInitCloner.swift

  Sources/Parse/Parse.swift
  Sources/Parse/Regex.swift

  Sources/SIL/ApplySite.swift
  Sources/SIL/Argument.swift
  Sources/SIL/BasicBlock.swift
  Sources/SIL/Builder.swift
  Sources/SIL/Effects.swift
  Sources/SIL/Function.swift
  Sources/SIL/GlobalVariable.swift
  Sources/SIL/Instruction.swift
  Sources/SIL/Location.swift
  Sources/SIL/Operand.swift
  Sources/SIL/Registration.swift
  Sources/SIL/SmallProjectionPath.swift
  Sources/SIL/SubstitutionMap.swift
  Sources/SIL/Type.swift
  Sources/SIL/Value.swift
  Sources/SIL/VTable.swift
  Sources/SIL/WitnessTable.swift

  DEPENDENCIES
    swiftAST
)
