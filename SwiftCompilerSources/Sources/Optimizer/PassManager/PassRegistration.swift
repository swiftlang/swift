//===--- PassRegistration.swift - Register optimization passes -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL
import OptimizerBridging

@_cdecl("initializeSwiftModules")
public func initializeSwiftModules() {
  registerAST()
  registerSILClasses()
  registerSwiftAnalyses()
  registerUtilities()
  registerSwiftPasses()
  registerOptimizerTests()
}

private func registerPass(
      _ pass: ModulePass,
      _ runFn: @escaping (@convention(c) (BridgedPassContext) -> ())) {
  pass.name._withBridgedStringRef { nameStr in
    SILPassManager_registerModulePass(nameStr, runFn)
  }
}

private func registerPass(
      _ pass: FunctionPass,
      _ runFn: @escaping (@convention(c) (BridgedFunctionPassCtxt) -> ())) {
  pass.name._withBridgedStringRef { nameStr in
    SILPassManager_registerFunctionPass(nameStr, runFn)
  }
}

protocol SILCombineSimplifiable : Instruction {
  func simplify(_ context: SimplifyContext)
}

private func run<InstType: SILCombineSimplifiable>(_ instType: InstType.Type,
                                                   _ bridgedCtxt: BridgedInstructionPassCtxt) {
  let inst = bridgedCtxt.instruction.getAs(instType)
  let context = SimplifyContext(_bridged: bridgedCtxt.passContext,
                                notifyInstructionChanged: {inst in},
                                preserveDebugInfo: false)
  inst.simplify(context)
}

private func registerForSILCombine<InstType: SILCombineSimplifiable>(
      _ instType: InstType.Type,
      _ runFn: @escaping (@convention(c) (BridgedInstructionPassCtxt) -> ())) {
  "\(instType)"._withBridgedStringRef { instClassStr in
    SILCombine_registerInstructionPass(instClassStr, runFn)
  }
}

private func registerSwiftPasses() {
  // Module passes
  registerPass(mandatoryAllocBoxToStack, { mandatoryAllocBoxToStack.run($0) })
  registerPass(mandatoryPerformanceOptimizations, { mandatoryPerformanceOptimizations.run($0) })
  registerPass(diagnoseUnknownConstValues, { diagnoseUnknownConstValues.run($0)})
  registerPass(readOnlyGlobalVariablesPass, { readOnlyGlobalVariablesPass.run($0) })
  registerPass(stackProtection, { stackProtection.run($0) })
  registerPass(embeddedSwiftDiagnostics, { embeddedSwiftDiagnostics.run($0) })

  // Function passes
  registerPass(allocBoxToStack, { allocBoxToStack.run($0) })
  registerPass(asyncDemotion, { asyncDemotion.run($0) })
  registerPass(booleanLiteralFolding, { booleanLiteralFolding.run($0) })
  registerPass(letPropertyLowering, { letPropertyLowering.run($0) })
  registerPass(mergeCondFailsPass, { mergeCondFailsPass.run($0) })
  registerPass(computeEscapeEffects, { computeEscapeEffects.run($0) })
  registerPass(computeSideEffects, { computeSideEffects.run($0) })
  registerPass(diagnoseInfiniteRecursion, { diagnoseInfiniteRecursion.run($0) })
  registerPass(destroyHoisting, { destroyHoisting.run($0) })
  registerPass(initializeStaticGlobalsPass, { initializeStaticGlobalsPass.run($0) })
  registerPass(objCBridgingOptimization, { objCBridgingOptimization.run($0) })
  registerPass(objectOutliner, { objectOutliner.run($0) })
  registerPass(stackPromotion, { stackPromotion.run($0) })
  registerPass(functionStackProtection, { functionStackProtection.run($0) })
  registerPass(assumeSingleThreadedPass, { assumeSingleThreadedPass.run($0) })
  registerPass(releaseDevirtualizerPass, { releaseDevirtualizerPass.run($0) })
  registerPass(simplificationPass, { simplificationPass.run($0) })
  registerPass(ononeSimplificationPass, { ononeSimplificationPass.run($0) })
  registerPass(lateOnoneSimplificationPass, { lateOnoneSimplificationPass.run($0) })
  registerPass(cleanupDebugStepsPass, { cleanupDebugStepsPass.run($0) })
  registerPass(namedReturnValueOptimization, { namedReturnValueOptimization.run($0) })
  registerPass(stripObjectHeadersPass, { stripObjectHeadersPass.run($0) })
  registerPass(deadStoreElimination, { deadStoreElimination.run($0) })
  registerPass(redundantLoadElimination, { redundantLoadElimination.run($0) })
  registerPass(mandatoryRedundantLoadElimination, { mandatoryRedundantLoadElimination.run($0) })
  registerPass(earlyRedundantLoadElimination, { earlyRedundantLoadElimination.run($0) })
  registerPass(deinitDevirtualizer, { deinitDevirtualizer.run($0) })
  registerPass(lifetimeDependenceDiagnosticsPass, { lifetimeDependenceDiagnosticsPass.run($0) })
  registerPass(lifetimeDependenceInsertionPass, { lifetimeDependenceInsertionPass.run($0) })
  registerPass(lifetimeDependenceScopeFixupPass, { lifetimeDependenceScopeFixupPass.run($0) })
  registerPass(copyToBorrowOptimization, { copyToBorrowOptimization.run($0) })
  registerPass(tempRValueElimination, { tempRValueElimination.run($0) })
  registerPass(mandatoryTempRValueElimination, { mandatoryTempRValueElimination.run($0) })
  registerPass(tempLValueElimination, { tempLValueElimination.run($0) })
  registerPass(generalClosureSpecialization, { generalClosureSpecialization.run($0) })
  registerPass(autodiffClosureSpecialization, { autodiffClosureSpecialization.run($0) })

  // Instruction passes
  registerForSILCombine(BeginBorrowInst.self,      { run(BeginBorrowInst.self, $0) })
  registerForSILCombine(BeginCOWMutationInst.self, { run(BeginCOWMutationInst.self, $0) })
  registerForSILCombine(FixLifetimeInst.self,      { run(FixLifetimeInst.self, $0) })
  registerForSILCombine(GlobalValueInst.self,      { run(GlobalValueInst.self, $0) })
  registerForSILCombine(StrongRetainInst.self,     { run(StrongRetainInst.self, $0) })
  registerForSILCombine(StrongReleaseInst.self,    { run(StrongReleaseInst.self, $0) })
  registerForSILCombine(RetainValueInst.self,      { run(RetainValueInst.self, $0) })
  registerForSILCombine(ReleaseValueInst.self,     { run(ReleaseValueInst.self, $0) })
  registerForSILCombine(LoadInst.self,             { run(LoadInst.self, $0) })
  registerForSILCombine(LoadBorrowInst.self,       { run(LoadBorrowInst.self, $0) })
  registerForSILCombine(CopyValueInst.self,        { run(CopyValueInst.self, $0) })
  registerForSILCombine(CopyBlockInst.self,        { run(CopyBlockInst.self, $0) })
  registerForSILCombine(DestroyValueInst.self,     { run(DestroyValueInst.self, $0) })
  registerForSILCombine(DestructureStructInst.self, { run(DestructureStructInst.self, $0) })
  registerForSILCombine(DestructureTupleInst.self, { run(DestructureTupleInst.self, $0) })
  registerForSILCombine(TypeValueInst.self, { run(TypeValueInst.self, $0) })
  registerForSILCombine(ClassifyBridgeObjectInst.self, { run(ClassifyBridgeObjectInst.self, $0) })
  registerForSILCombine(MarkDependenceInst.self,    { run(MarkDependenceInst.self, $0) })
  registerForSILCombine(MarkDependenceAddrInst.self, { run(MarkDependenceAddrInst.self, $0) })
  registerForSILCombine(PointerToAddressInst.self,  { run(PointerToAddressInst.self, $0) })
  registerForSILCombine(UncheckedEnumDataInst.self, { run(UncheckedEnumDataInst.self, $0) })
  registerForSILCombine(WitnessMethodInst.self,     { run(WitnessMethodInst.self, $0) })
  registerForSILCombine(UncheckedAddrCastInst.self, { run(UncheckedAddrCastInst.self, $0) })
  registerForSILCombine(UnconditionalCheckedCastInst.self, { run(UnconditionalCheckedCastInst.self, $0) })
  registerForSILCombine(AllocStackInst.self,        { run(AllocStackInst.self, $0) })
  registerForSILCombine(ApplyInst.self,             { run(ApplyInst.self, $0) })
  registerForSILCombine(TryApplyInst.self,          { run(TryApplyInst.self, $0) })

  // Test passes
  registerPass(aliasInfoDumper, { aliasInfoDumper.run($0) })
  registerPass(functionUsesDumper, { functionUsesDumper.run($0) })
  registerPass(silPrinterPass, { silPrinterPass.run($0) })
  registerPass(escapeInfoDumper, { escapeInfoDumper.run($0) })
  registerPass(addressEscapeInfoDumper, { addressEscapeInfoDumper.run($0) })
  registerPass(accessDumper, { accessDumper.run($0) })
  registerPass(deadEndBlockDumper, { deadEndBlockDumper.run($0) })
  registerPass(memBehaviorDumper, { memBehaviorDumper.run($0) })
  registerPass(rangeDumper, { rangeDumper.run($0) })
  registerPass(runUnitTests, { runUnitTests.run($0) })
  registerPass(testInstructionIteration, { testInstructionIteration.run($0) })
  registerPass(updateBorrowedFromPass, { updateBorrowedFromPass.run($0) })
}

private func registerSwiftAnalyses() {
  AliasAnalysis.register()
  CalleeAnalysis.register()
}

private func registerUtilities() {
  registerVerifier()
  registerPhiUpdater()
}
