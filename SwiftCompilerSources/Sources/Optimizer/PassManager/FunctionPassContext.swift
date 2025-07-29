//===--- FunctionPassContext.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

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
