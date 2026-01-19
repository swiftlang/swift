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

import AST
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
  
  var loopTree: LoopTree {
    let bridgedLT = bridgedPassContext.getLoopTree()
    return LoopTree(bridged: bridgedLT, context: self)
  }

  /// Updates all analysis.
  /// This is useful if a pass needs an updated analysis after invalidating the SIL of a function.
  func updateAnalysis() {
    bridgedPassContext.updateAnalysis()
  }

  func notifyNewFunction(function: Function, derivedFrom: Function) {
    bridgedPassContext.addFunctionToPassManagerWorklist(function.bridged, derivedFrom.bridged)
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

  func specialize(function: Function,
                  for substitutions: SubstitutionMap,
                  convertIndirectToDirect: Bool,
                  isMandatory: Bool
  ) -> Function? {
    return bridgedPassContext.specializeFunction(function.bridged, substitutions.bridged,
                                                 convertIndirectToDirect, isMandatory).function
  }

  func mangleOutlinedVariable(from function: Function) -> String {
    return String(taking: bridgedPassContext.mangleOutlinedVariable(function.bridged))
  }

  enum ClosureArgumentMangling {
    case closure(SingleValueInstruction)

    /// The argument specializes for the same closure as a previous argument, e.g.
    /// ```
    ///   %1 = partial_apply %closure
    ///   apply %f(%1, %1)    // first argument: `.closure(%1)`
    ///                       // second argument: `.previousArgumentIndex(0)`
    case previousArgumentIndex(Int)
  }

  func mangle(withClosureArguments closureArgs: [(argumentIndex: Int, argumentValue: ClosureArgumentMangling)],
              from applySiteCallee: Function
  ) -> String {
    let bridgedArgManglings = closureArgs.map {
      switch $0.argumentValue {
      case .closure(let closure):
        return BridgedPassContext.ClosureArgMangling(argIdx: $0.argumentIndex,
                                                     inst: Optional<Instruction>(closure).bridged,
                                                     otherArgIdx: -1)
      case .previousArgumentIndex(let idx):
        return BridgedPassContext.ClosureArgMangling(argIdx: $0.argumentIndex,
                                                     inst: OptionalBridgedInstruction(),
                                                     otherArgIdx: idx)
      }
    }

    return bridgedArgManglings.withBridgedArrayRef{ bridgedClosureArgs in
      String(taking: bridgedPassContext.mangleWithClosureArgs(bridgedClosureArgs, applySiteCallee.bridged))
    }
  }

  func mangle(withConstantCaptureArguments constArgs: [(argumentIndex: Int, argument: Value)],
              from applySiteCallee: Function
  ) -> String {
    let bridgedConstArgs = constArgs.map { ($0.argumentIndex, $0.argument.bridged) }
    return bridgedConstArgs.withBridgedArrayRef{ bridgedConstArgsArray in
      String(taking: bridgedPassContext.mangleWithConstCaptureArgs(bridgedConstArgsArray, applySiteCallee.bridged))
    }
  }

  func mangle(withBoxToStackPromotedArguments argIndices: [Int], from original: Function) -> String {
    argIndices.withBridgedArrayRef { bridgedArgIndices in
      String(taking: bridgedPassContext.mangleWithBoxToStackPromotedArgs(bridgedArgIndices, original.bridged))
    }
  }

  func mangle(withExplodedPackArguments argIndices: [Int], from original: Function) -> String {
    return argIndices.withBridgedArrayRef { bridgedArgIndices in
      String(taking: bridgedPassContext.mangleWithExplodedPackArgs(bridgedArgIndices, original.bridged))
    }
  }

  func mangle(withChangedRepresentation original: Function) -> String {
    String(taking: bridgedPassContext.mangleWithChangedRepresentation(original.bridged))
  }

  func createSpecializedFunctionDeclaration(
    from original: Function, withName specializedFunctionName: String,
    withParams specializedParameters: [ParameterInfo],
    withResults specializedResults: [ResultInfo]? = nil,
    withRepresentation: FunctionTypeRepresentation? = nil,
    makeBare: Bool = false,
    preserveGenericSignature: Bool = true
  ) -> Function {
    return specializedFunctionName._withBridgedStringRef { nameRef in
      let bridgedParamInfos = specializedParameters.map { $0._bridged }
      let repr = withRepresentation ?? original.loweredFunctionType.functionTypeRepresentation

      return bridgedParamInfos.withUnsafeBufferPointer { paramBuf in

        if let bridgedResultInfos = specializedResults?.map({ $0._bridged }) {

          return bridgedResultInfos.withUnsafeBufferPointer { resultBuf in
            return bridgedPassContext.createSpecializedFunctionDeclaration(
              nameRef, paramBuf.baseAddress, paramBuf.count,
              resultBuf.baseAddress, resultBuf.count,
              original.bridged, repr.bridged, makeBare,
              preserveGenericSignature
            ).function
          }
        } else {
          return bridgedPassContext.createSpecializedFunctionDeclaration(
            nameRef, paramBuf.baseAddress, paramBuf.count,
            nil, 0,
            original.bridged, repr.bridged, makeBare,
            preserveGenericSignature
          ).function
        }
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

  /// True if the pass has notified that stack nesting needs to be fixed.
  /// In this case the pass needs to call `fixStackNesting`.
  var needFixStackNesting: Bool { bridgedPassContext.getNeedFixStackNesting() }

  /// True if the pass has deleted any basic blocks.
  /// In case a deleted block was an exit blocks of a loop the remaining loop might have become an infinite
  /// loop. Infinite loops must be eliminated by calling `breakInfiniteLoops`.
  var needBreakInfiniteLoops: Bool { bridgedPassContext.getNeedBreakInfiniteLoops() }

  /// If `value` is true, notifies that a pass has deleted a basic block which might end up in an infinite loop.
  /// A pass can set the notification to `false` again if a basic block has been deleted but the pass knows
  /// that this cannot cause any infinite loops.
  func setNeedBreakInfiniteLoops(to value: Bool) { bridgedPassContext.setNeedBreakInfiniteLoops(value) }

  /// True if the pass has inserted an `unreachable` instruction.
  /// In such a case, lifetimes which have reached over this point are cut off and must be completed
  /// by calling `completeLifetimes`.
  var needCompleteLifetimes: Bool { bridgedPassContext.getNeedCompleteLifetimes() }

  /// If `value` is true, notifies that a pass has inserted an `unreachable` instruction which might
  /// have cut off any lifetimes.
  /// A pass can set the notification to `false` again if an `unreachable` was inserted but the pass
  /// knows that this didn't cut off any lifetimes.
  func setNeedCompleteLifetimes(to value: Bool) { bridgedPassContext.setNeedCompleteLifetimes(value) }

  func fixStackNesting(in function: Function) {
    bridgedPassContext.fixStackNesting(function.bridged)
  }
}
