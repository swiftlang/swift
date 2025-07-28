//===--- Context.swift - defines the context types ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL
import OptimizerBridging

extension Context {
  var bridgedPassContext: BridgedPassContext {
    BridgedPassContext(_bridged)
  }

  var options: Options { Options(_bridged: bridgedPassContext) }

  var diagnosticEngine: DiagnosticEngine {
    return DiagnosticEngine(bridged: bridgedPassContext.getDiagnosticEngine())
  }

  // The calleeAnalysis is not specific to a function and therefore can be provided in
  // all contexts.
  var calleeAnalysis: CalleeAnalysis {
    let bridgeCA = bridgedPassContext.getCalleeAnalysis()
    return CalleeAnalysis(bridged: bridgeCA)
  }

  var hadError: Bool { bridgedPassContext.hadError() }

  /// Enable diagnostics requiring WMO (for @noLocks, @noAllocation
  /// annotations, Embedded Swift, and class specialization). SourceKit is the
  /// only consumer that has this disabled today (as it disables WMO
  /// explicitly).
  var enableWMORequiredDiagnostics: Bool {
    bridgedPassContext.enableWMORequiredDiagnostics()
  }

  func canMakeStaticObjectReadOnly(objectType: Type) -> Bool {
    bridgedPassContext.canMakeStaticObjectReadOnly(objectType.bridged)
  }

  func notifyNewFunction(function: Function, derivedFrom: Function) {
    bridgedPassContext.addFunctionToPassManagerWorklist(function.bridged, derivedFrom.bridged)
  }
}

extension MutatingContext {
  func notifyInvalidatedStackNesting() { bridgedPassContext.notifyInvalidatedStackNesting() }
  var needFixStackNesting: Bool { bridgedPassContext.getNeedFixStackNesting() }

  func tryOptimizeApplyOfPartialApply(closure: PartialApplyInst) -> Bool {
    if bridgedPassContext.tryOptimizeApplyOfPartialApply(closure.bridged) {
      notifyInstructionsChanged()
      notifyCallsChanged()

      for use in closure.callee.uses {
        if use.instruction is FullApplySite {
          notifyInstructionChanged(use.instruction)
        }
      }
      return true
    }
    return false
  }

  func tryDeleteDeadClosure(closure: SingleValueInstruction, needKeepArgsAlive: Bool = true) -> Bool {
    if bridgedPassContext.tryDeleteDeadClosure(closure.bridged, needKeepArgsAlive) {
      notifyInstructionsChanged()
      return true
    }
    return false
  }

  func tryDevirtualize(apply: FullApplySite, isMandatory: Bool) -> ApplySite? {
    let result = bridgedPassContext.tryDevirtualizeApply(apply.bridged, isMandatory)
    if let newApply = result.newApply.instruction {
      erase(instruction: apply)
      notifyInstructionsChanged()
      notifyCallsChanged()
      if result.cfgChanged {
        notifyBranchesChanged()
      }
      notifyInstructionChanged(newApply)
      return newApply as! FullApplySite
    }
    return nil
  }
  
  func tryOptimizeKeypath(apply: FullApplySite) -> Bool {
    return bridgedPassContext.tryOptimizeKeypath(apply.bridged)
  }

  func inlineFunction(apply: FullApplySite, mandatoryInline: Bool) {
    // This is only a best-effort attempt to notify the new cloned instructions as changed.
    // TODO: get a list of cloned instructions from the `inlineFunction`
    let instAfterInling: Instruction?
    switch apply {
    case is ApplyInst:
      instAfterInling = apply.next
    case let beginApply as BeginApplyInst:
      let next = beginApply.next!
      instAfterInling = (next is EndApplyInst ? nil : next)
    case is TryApplyInst:
      instAfterInling = apply.parentBlock.next?.instructions.first
    default:
      instAfterInling = nil
    }

    bridgedPassContext.inlineFunction(apply.bridged, mandatoryInline)

    if let instAfterInling = instAfterInling {
      notifyNewInstructions(from: apply, to: instAfterInling)
    }
  }

  func loadFunction(function: Function, loadCalleesRecursively: Bool) -> Bool {
    if function.isDefinition {
      return true
    }
    _bridged.loadFunction(function.bridged, loadCalleesRecursively)
    return function.isDefinition
  }

  private func notifyNewInstructions(from: Instruction, to: Instruction) {
    var inst = from
    while inst != to {
      if !inst.isDeleted {
        notifyInstructionChanged(inst)
      }
      if let next = inst.next {
        inst = next
      } else {
        inst = inst.parentBlock.next!.instructions.first!
      }
    }
  }

  func getContextSubstitutionMap(for type: Type) -> SubstitutionMap {
    SubstitutionMap(bridged: _bridged.getContextSubstitutionMap(type.bridged))
  }

  /// Notifies the pass manager that the optimization result of the current pass depends
  /// on the body (i.e. SIL instructions) of another function than the currently optimized one.
  func notifyDependency(onBodyOf otherFunction: Function) {
    bridgedPassContext.notifyDependencyOnBodyOf(otherFunction.bridged)
  }
}

/// The context which is passed to the run-function of a FunctionPass.
struct FunctionPassContext : MutatingContext {
  let _bridged: BridgedContext

  // A no-op.
  var notifyInstructionChanged: (Instruction) -> () { return { inst in } }

  func continueWithNextSubpassRun(for inst: Instruction? = nil) -> Bool {
    return bridgedPassContext.continueWithNextSubpassRun(inst.bridged)
  }

  func createSimplifyContext(preserveDebugInfo: Bool,
                             notifyInstructionChanged: @escaping (Instruction) -> ()
  ) -> SimplifyContext {
    SimplifyContext(_bridged: _bridged, notifyInstructionChanged: notifyInstructionChanged,
                    preserveDebugInfo: preserveDebugInfo)
  }

  var deadEndBlocks: DeadEndBlocksAnalysis {
    let bridgeDEA = bridgedPassContext.getDeadEndBlocksAnalysis()
    return DeadEndBlocksAnalysis(bridged: bridgeDEA)
  }

  var dominatorTree: DominatorTree {
    let bridgedDT = bridgedPassContext.getDomTree()
    return DominatorTree(bridged: bridgedDT)
  }

  var postDominatorTree: PostDominatorTree {
    let bridgedPDT = bridgedPassContext.getPostDomTree()
    return PostDominatorTree(bridged: bridgedPDT)
  }

  func loadFunction(name: StaticString, loadCalleesRecursively: Bool) -> Function? {
    return name.withUTF8Buffer { (nameBuffer: UnsafeBufferPointer<UInt8>) in
      let nameStr = BridgedStringRef(data: nameBuffer.baseAddress, count: nameBuffer.count)
      return _bridged.loadFunction(nameStr, loadCalleesRecursively).function
    }
  }

  func eliminateDeadAllocations(in function: Function) -> Bool {
    if bridgedPassContext.eliminateDeadAllocations(function.bridged) {
      notifyInstructionsChanged()
      return true
    }
    return false
  }

  func specializeClassMethodInst(_ cm: ClassMethodInst) -> Bool {
    if bridgedPassContext.specializeClassMethodInst(cm.bridged) {
      notifyInstructionsChanged()
      notifyCallsChanged()
      return true
    }
    return false
  }

  func specializeWitnessMethodInst(_ wm: WitnessMethodInst) -> Bool {
    if bridgedPassContext.specializeWitnessMethodInst(wm.bridged) {
      notifyInstructionsChanged()
      notifyCallsChanged()
      return true
    }
    return false
  }

  func specializeApplies(in function: Function, isMandatory: Bool) -> Bool {
    if bridgedPassContext.specializeAppliesInFunction(function.bridged, isMandatory) {
      notifyInstructionsChanged()
      notifyCallsChanged()
      return true
    }
    return false
  }

  func mangleOutlinedVariable(from function: Function) -> String {
    return String(taking: bridgedPassContext.mangleOutlinedVariable(function.bridged))
  }

  func mangle(withClosureArguments closureArgs: [Value], closureArgIndices: [Int], from applySiteCallee: Function) -> String {
    closureArgs.withBridgedValues { bridgedClosureArgsRef in
      closureArgIndices.withBridgedArrayRef{bridgedClosureArgIndicesRef in 
        String(taking: bridgedPassContext.mangleWithClosureArgs(
          bridgedClosureArgsRef,
          bridgedClosureArgIndicesRef, 
          applySiteCallee.bridged
        ))
      }
    }
  }

  func mangle(withBoxToStackPromotedArguments argIndices: [Int], from original: Function) -> String {
    argIndices.withBridgedArrayRef { bridgedArgIndices in
      String(taking: bridgedPassContext.mangleWithBoxToStackPromotedArgs(bridgedArgIndices, original.bridged))
    }
  }

  func createSpecializedFunctionDeclaration(from original: Function, withName specializedFunctionName: String,
                                            withParams specializedParameters: [ParameterInfo],
                                            makeThin: Bool = false,
                                            makeBare: Bool = false) -> Function
  {
    return specializedFunctionName._withBridgedStringRef { nameRef in
      let bridgedParamInfos = specializedParameters.map { $0._bridged }

      return bridgedParamInfos.withUnsafeBufferPointer { paramBuf in
        bridgedPassContext.createSpecializedFunctionDeclaration(nameRef, paramBuf.baseAddress, paramBuf.count,
                                                                original.bridged, makeThin, makeBare).function
      }
    }
  }

  func buildSpecializedFunction<T>(specializedFunction: Function, buildFn: (Function, FunctionPassContext) -> T) -> T {
    let nestedBridgedContext = bridgedPassContext.initializeNestedPassContext(specializedFunction.bridged)
    let nestedContext = FunctionPassContext(_bridged: nestedBridgedContext)
    defer { bridgedPassContext.deinitializedNestedPassContext() }

    return buildFn(specializedFunction, nestedContext)
  }

  /// Makes sure that the lifetime of `value` ends at all control flow paths, even in dead-end blocks.
  /// Inserts destroys in dead-end blocks if those are missing.
  func completeLifetime(of value: Value) {
    if bridgedPassContext.completeLifetime(value.bridged) {
      notifyInstructionsChanged()
    }
  }

  func fixStackNesting(in function: Function) {
    bridgedPassContext.fixStackNesting(function.bridged)
  }
}

struct SimplifyContext : MutatingContext {
  let _bridged: BridgedContext
  let notifyInstructionChanged: (Instruction) -> ()
  let preserveDebugInfo: Bool

  func constantFold(builtin: BuiltinInst) -> Value? {
    bridgedPassContext.constantFoldBuiltin(builtin.bridged).value
  }
}

// Those Type APIs get information from IRGenModule, so they are formally not part of SIL and
// therefore need to be defined here, in the Optimizer module.
extension Type {
  func getStaticSize(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticSize(self.bridged)
    return v == -1 ? nil : v
  }
  
  func getStaticAlignment(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticAlignment(self.bridged)
    return v == -1 ? nil : v
  }
  
  func getStaticStride(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticStride(self.bridged)
    return v == -1 ? nil : v
  }
}
