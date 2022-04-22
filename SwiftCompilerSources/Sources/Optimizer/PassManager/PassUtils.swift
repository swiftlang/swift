//===--- PassUtils.swift - Utilities for optimzation passes ---------------===//
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

import SIL
import OptimizerBridging

public typealias BridgedFunctionPassCtxt =
  OptimizerBridging.BridgedFunctionPassCtxt
public typealias BridgedInstructionPassCtxt =
  OptimizerBridging.BridgedInstructionPassCtxt

struct PassContext {

  let _bridged: BridgedPassContext

  var aliasAnalysis: AliasAnalysis {
    let bridgedAA = PassContext_getAliasAnalysis(_bridged)
    return AliasAnalysis(bridged: bridgedAA)
  }

  var calleeAnalysis: CalleeAnalysis {
    let bridgeCA = PassContext_getCalleeAnalysis(_bridged)
    return CalleeAnalysis(bridged: bridgeCA)
  }

  var deadEndBlocks: DeadEndBlocksAnalysis {
    let bridgeDEA = PassContext_getDeadEndBlocksAnalysis(_bridged)
    return DeadEndBlocksAnalysis(bridged: bridgeDEA)
  }

  var dominatorTree: DominatorTree {
    let bridgedDT = PassContext_getDomTree(_bridged)
    return DominatorTree(bridged: bridgedDT)
  }

  var postDominatorTree: PostDominatorTree {
    let bridgedPDT = PassContext_getPostDomTree(_bridged)
    return PostDominatorTree(bridged: bridgedPDT)
  }

  func notifyInstructionsChanged() {
    PassContext_notifyChanges(_bridged, instructionsChanged)
  }

  private func notifyCallsChanged() {
    PassContext_notifyChanges(_bridged, callsChanged)
  }

  private func notifyBranchesChanged() {
    PassContext_notifyChanges(_bridged, branchesChanged)
  }

  enum EraseMode {
    case onlyInstruction, includingDebugUses
  }

  func erase(instruction: Instruction, _ mode: EraseMode = .onlyInstruction) {
    switch mode {
      case .onlyInstruction:
        break
      case .includingDebugUses:
        for result in instruction.results {
          for use in result.uses {
            assert(use.instruction is DebugValueInst)
            PassContext_eraseInstruction(_bridged, use.instruction.bridged)
          }
        }
    }

    if instruction is FullApplySite {
      notifyCallsChanged()
    }
    if instruction is TermInst {
      notifyBranchesChanged()
    }
    notifyInstructionsChanged()

    PassContext_eraseInstruction(_bridged, instruction.bridged)
  }

  func replaceAllUses(of value: Value, with replacement: Value) {
    for use in value.uses {
      setOperand(of: use.instruction, at: use.index, to: replacement)
    }
  }

  func setOperand(of instruction: Instruction, at index : Int, to value: Value) {
    if instruction is FullApplySite && index == ApplyOperands.calleeOperandIndex {
      notifyCallsChanged()
    }
    notifyInstructionsChanged()

    SILInstruction_setOperand(instruction.bridged, index, value.bridged)
  }

  func setAtomicity(of instruction: RefCountingInst, isAtomic: Bool) {
    PassContext_notifyChanges(_bridged, instructionsChanged)

    RefCountingInst_setIsAtomic(instruction.bridged, isAtomic)
  }
  
  func fixStackNesting(function: Function) {
    PassContext_fixStackNesting(_bridged, function.bridged)
  }

  func getContextSubstitutionMap(for type: Type) -> SubstitutionMap {
    SubstitutionMap(PassContext_getContextSubstitutionMap(_bridged, type.bridged))
  }

  func modifyEffects(in function: Function, _ body: (inout FunctionEffects) -> ()) {
    function._modifyEffects(body)
    // TODO: do we need to notify any changes?
  }
}

struct FunctionPass {

  let name: String
  let runFunction: (Function, PassContext) -> ()

  public init(name: String,
              _ runFunction: @escaping (Function, PassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
  }

  func run(_ bridgedCtxt: BridgedFunctionPassCtxt) {
    let function = bridgedCtxt.function.function
    let context = PassContext(_bridged: bridgedCtxt.passContext)
    runFunction(function, context)
  }
}

struct InstructionPass<InstType: Instruction> {

  let name: String
  let runFunction: (InstType, PassContext) -> ()

  public init(name: String,
              _ runFunction: @escaping (InstType, PassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
  }

  func run(_ bridgedCtxt: BridgedInstructionPassCtxt) {
    let inst = bridgedCtxt.instruction.getAs(InstType.self)
    let context = PassContext(_bridged: bridgedCtxt.passContext)
    runFunction(inst, context)
  }
}

extension Builder {
  init(at insPnt: Instruction, location: Location,
       _ context: PassContext) {
    self.init(insertionPoint: insPnt, location: location,
              passContext: context._bridged)
  }

  init(at insPnt: Instruction, _ context: PassContext) {
    self.init(insertionPoint: insPnt, location: insPnt.location,
              passContext: context._bridged)
  }
}

extension AllocRefInstBase {
  func setIsStackAllocatable(_ context: PassContext) {
    AllocRefInstBase_setIsStackAllocatable(bridged)
    context.notifyInstructionsChanged()
  }
}
