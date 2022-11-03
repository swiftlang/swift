//===--- PassContext.swift - defines the PassContext type -----------------===//
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

import SIL
import OptimizerBridging

public typealias BridgedFunctionPassCtxt =
  OptimizerBridging.BridgedFunctionPassCtxt
public typealias BridgedInstructionPassCtxt =
  OptimizerBridging.BridgedInstructionPassCtxt

struct PassContext {

  let _bridged: BridgedPassContext

  var options: Options { Options(_bridged: _bridged) }

  func continueWithNextSubpassRun(for inst: Instruction? = nil) -> Bool {
    let bridgedInst = OptionalBridgedInstruction(obj: inst?.bridged.obj)
    return PassContext_continueWithNextSubpassRun(_bridged, bridgedInst) != 0
  }

  //===--------------------------------------------------------------------===//
  //                               Analysis
  //===--------------------------------------------------------------------===//

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

  //===--------------------------------------------------------------------===//
  //                   Interaction with AST and the SIL module
  //===--------------------------------------------------------------------===//

  func getContextSubstitutionMap(for type: Type) -> SubstitutionMap {
    SubstitutionMap(PassContext_getContextSubstitutionMap(_bridged, type.bridged))
  }

  func loadFunction(name: StaticString) -> Function? {
    return name.withUTF8Buffer { (nameBuffer: UnsafeBufferPointer<UInt8>) in
      PassContext_loadFunction(_bridged, llvm.StringRef(nameBuffer.baseAddress, nameBuffer.count)).function
    }
  }

  //===--------------------------------------------------------------------===//
  //                             Modify SIL
  //===--------------------------------------------------------------------===//

  /// Splits the basic block, which contains `inst`, before `inst` and returns the
  /// new block.
  ///
  /// `inst` and all subsequent instructions are moved to the new block, while all
  /// instructions _before_ `inst` remain in the original block.
  func splitBlock(at inst: Instruction) -> BasicBlock {
    notifyBranchesChanged()
    return PassContext_splitBlock(inst.bridged).block
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

  func fixStackNesting(function: Function) {
    PassContext_fixStackNesting(_bridged, function.bridged)
  }

  func modifyEffects(in function: Function, _ body: (inout FunctionEffects) -> ()) {
    notifyEffectsChanged()
    function._modifyEffects(body)
  }

  //===--------------------------------------------------------------------===//
  //                          Private utilities
  //===--------------------------------------------------------------------===//

  fileprivate func notifyInstructionsChanged() {
    PassContext_notifyChanges(_bridged, instructionsChanged)
  }

  fileprivate func notifyCallsChanged() {
    PassContext_notifyChanges(_bridged, callsChanged)
  }

  fileprivate func notifyBranchesChanged() {
    PassContext_notifyChanges(_bridged, branchesChanged)
  }

  fileprivate func notifyEffectsChanged() {
    PassContext_notifyChanges(_bridged, effectsChanged)
  }
}

//===----------------------------------------------------------------------===//
//                          Builder initialization
//===----------------------------------------------------------------------===//

extension Builder {
  /// Creates a builder which inserts _before_ `insPnt`, using a custom `location`.
  init(at insPnt: Instruction, location: Location, _ context: PassContext) {
    self.init(insertAt: .before(insPnt), location: location, passContext: context._bridged)
  }

  /// Creates a builder which inserts _before_ `insPnt`, using the location of `insPnt`.
  init(at insPnt: Instruction, _ context: PassContext) {
    self.init(insertAt: .before(insPnt), location: insPnt.location, passContext: context._bridged)
  }

  /// Creates a builder which inserts _after_ `insPnt`, using a custom `location`.
  init(after insPnt: Instruction, location: Location, _ context: PassContext) {
    if let nextInst = insPnt.next {
      self.init(insertAt: .before(nextInst), location: location, passContext: context._bridged)
    } else {
      self.init(insertAt: .atEndOf(insPnt.block), location: location, passContext: context._bridged)
    }
  }

  /// Creates a builder which inserts _after_ `insPnt`, using the location of `insPnt`.
  init(after insPnt: Instruction, _ context: PassContext) {
    self.init(after: insPnt, location: insPnt.location, context)
  }

  /// Creates a builder which inserts at the end of `block`, using a custom `location`.
  init(atEndOf block: BasicBlock, location: Location, _ context: PassContext) {
    self.init(insertAt: .atEndOf(block), location: location, passContext: context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using a custom `location`.
  init(atBeginOf block: BasicBlock, location: Location, _ context: PassContext) {
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst), location: location, passContext: context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using the location of the first
  /// instruction of `block`.
  init(atBeginOf block: BasicBlock, _ context: PassContext) {
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst), location: firstInst.location, passContext: context._bridged)
  }
}

//===----------------------------------------------------------------------===//
//                          Modifying the SIL
//===----------------------------------------------------------------------===//

extension BasicBlock {
  func addBlockArgument(type: Type, ownership: Ownership, _ context: PassContext) -> BlockArgument {
    context.notifyInstructionsChanged()
    return SILBasicBlock_addBlockArgument(bridged, type.bridged, ownership._bridged).blockArgument
  }
  
  func eraseArgument(at index: Int, _ context: PassContext) {
    context.notifyInstructionsChanged()
    SILBasicBlock_eraseArgument(bridged, index)
  }
}

extension AllocRefInstBase {
  func setIsStackAllocatable(_ context: PassContext) {
    context.notifyInstructionsChanged()
    AllocRefInstBase_setIsStackAllocatable(bridged)
  }
}

extension UseList {
  func replaceAll(with replacement: Value, _ context: PassContext) {
    for use in self {
      use.instruction.setOperand(at: use.index, to: replacement, context)
    }
  }
}

extension Instruction {
  func setOperand(at index : Int, to value: Value, _ context: PassContext) {
    if self is FullApplySite && index == ApplyOperands.calleeOperandIndex {
      context.notifyCallsChanged()
    }
    context.notifyInstructionsChanged()

    SILInstruction_setOperand(bridged, index, value.bridged)
  }
}

extension RefCountingInst {
  func setAtomicity(isAtomic: Bool, _ context: PassContext) {
    context.notifyInstructionsChanged()
    RefCountingInst_setIsAtomic(bridged, isAtomic)
  }
}

extension Function {
  func set(needStackProtection: Bool, _ context: PassContext) {
    context.notifyEffectsChanged()
    SILFunction_setNeedStackProtection(bridged, needStackProtection ? 1 : 0)
  }
}
