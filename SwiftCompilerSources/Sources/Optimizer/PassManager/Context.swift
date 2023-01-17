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

import SIL
import OptimizerBridging

/// The base type of all contexts.
protocol Context {
  var _bridged: BridgedPassContext { get }
}

extension Context {
  var options: Options { Options(_bridged: _bridged) }

  // The calleeAnalysis is not specific to a function and therefore can be provided in
  // all contexts.
  var calleeAnalysis: CalleeAnalysis {
    let bridgeCA = PassContext_getCalleeAnalysis(_bridged)
    return CalleeAnalysis(bridged: bridgeCA)
  }
}

/// A context which allows mutation of a function's SIL.
protocol MutatingContext : Context {
  // Called by all instruction mutations, including inserted new instructions.
  var notifyInstructionChanged: (Instruction) -> () { get }
}

extension MutatingContext {
  func notifyInvalidatedStackNesting() { PassContext_notifyInvalidatedStackNesting(_bridged) }
  var needFixStackNesting: Bool { PassContext_getNeedFixStackNesting(_bridged) }

  /// Splits the basic block, which contains `inst`, before `inst` and returns the
  /// new block.
  ///
  /// `inst` and all subsequent instructions are moved to the new block, while all
  /// instructions _before_ `inst` remain in the original block.
  func splitBlock(at inst: Instruction) -> BasicBlock {
    notifyBranchesChanged()
    return PassContext_splitBlock(inst.bridged).block
  }

  func erase(instruction: Instruction) {
    if instruction is FullApplySite {
      notifyCallsChanged()
    }
    if instruction is TermInst {
      notifyBranchesChanged()
    }
    notifyInstructionsChanged()

    PassContext_eraseInstruction(_bridged, instruction.bridged)
  }

  func erase(instructionIncludingDebugUses inst: Instruction) {
    for result in inst.results {
      for use in result.uses {
        assert(use.instruction is DebugValueInst)
        erase(instruction: use.instruction)
      }
    }
    erase(instruction: inst)
  }

  func tryDeleteDeadClosure(closure: SingleValueInstruction) -> Bool {
    PassContext_tryDeleteDeadClosure(_bridged, closure.bridged)
  }

  func getContextSubstitutionMap(for type: Type) -> SubstitutionMap {
    SubstitutionMap(PassContext_getContextSubstitutionMap(_bridged, type.bridged))
  }

  // Private utilities

  fileprivate func notifyInstructionsChanged() {
    PassContext_notifyChanges(_bridged, instructionsChanged)
  }

  fileprivate func notifyCallsChanged() {
    PassContext_notifyChanges(_bridged, callsChanged)
  }

  fileprivate func notifyBranchesChanged() {
    PassContext_notifyChanges(_bridged, branchesChanged)
  }
}

/// The context which is passed to the run-function of a FunctionPass.
struct FunctionPassContext : MutatingContext {
  let _bridged: BridgedPassContext

  // A no-op.
  var notifyInstructionChanged: (Instruction) -> () { return { inst in } }

  func continueWithNextSubpassRun(for inst: Instruction? = nil) -> Bool {
    let bridgedInst = OptionalBridgedInstruction(obj: inst?.bridged.obj)
    return PassContext_continueWithNextSubpassRun(_bridged, bridgedInst) != 0
  }

  func createSimplifyContext(preserveDebugInfo: Bool, notifyInstructionChanged: @escaping (Instruction) -> ()) -> SimplifyContext {
    SimplifyContext(_bridged: _bridged, notifyInstructionChanged: notifyInstructionChanged, preserveDebugInfo: preserveDebugInfo)
  }

  var aliasAnalysis: AliasAnalysis {
    let bridgedAA = PassContext_getAliasAnalysis(_bridged)
    return AliasAnalysis(bridged: bridgedAA)
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

  func loadFunction(name: StaticString) -> Function? {
    return name.withUTF8Buffer { (nameBuffer: UnsafeBufferPointer<UInt8>) in
      PassContext_loadFunction(_bridged, llvm.StringRef(nameBuffer.baseAddress, nameBuffer.count)).function
    }
  }

  func erase(block: BasicBlock) {
    PassContext_eraseBlock(_bridged, block.bridged)
  }

  func modifyEffects(in function: Function, _ body: (inout FunctionEffects) -> ()) {
    notifyEffectsChanged()
    function._modifyEffects(body)
  }

  fileprivate func notifyEffectsChanged() {
    PassContext_notifyChanges(_bridged, effectsChanged)
  }
}

struct SimplifyContext : MutatingContext {
  let _bridged: BridgedPassContext
  let notifyInstructionChanged: (Instruction) -> ()
  let preserveDebugInfo: Bool
}

//===----------------------------------------------------------------------===//
//                          Builder initialization
//===----------------------------------------------------------------------===//

extension Builder {
  /// Creates a builder which inserts _before_ `insPnt`, using a custom `location`.
  init(before insPnt: Instruction, location: Location, _ context: some MutatingContext) {
    self.init(insertAt: .before(insPnt), location: location,
              context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _before_ `insPnt`, using the location of `insPnt`.
  init(before insPnt: Instruction, _ context: some MutatingContext) {
    self.init(insertAt: .before(insPnt), location: insPnt.location,
              context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _after_ `insPnt`, using a custom `location`.
  init(after insPnt: Instruction, location: Location, _ context: some MutatingContext) {
    if let nextInst = insPnt.next {
      self.init(insertAt: .before(nextInst), location: location,
                context.notifyInstructionChanged, context._bridged)
    } else {
      self.init(insertAt: .atEndOf(insPnt.parentBlock), location: location,
                context.notifyInstructionChanged, context._bridged)
    }
  }

  /// Creates a builder which inserts _after_ `insPnt`, using the location of `insPnt`.
  init(after insPnt: Instruction, _ context: some MutatingContext) {
    self.init(after: insPnt, location: insPnt.location, context)
  }

  /// Creates a builder which inserts at the end of `block`, using a custom `location`.
  init(atEndOf block: BasicBlock, location: Location, _ context: some MutatingContext) {
    self.init(insertAt: .atEndOf(block), location: location,
              context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using a custom `location`.
  init(atBeginOf block: BasicBlock, location: Location, _ context: some MutatingContext) {
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst), location: location,
              context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using the location of the first
  /// instruction of `block`.
  init(atBeginOf block: BasicBlock, _ context: some MutatingContext) {
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst), location: firstInst.location,
              context.notifyInstructionChanged, context._bridged)
  }
}

//===----------------------------------------------------------------------===//
//                          Modifying the SIL
//===----------------------------------------------------------------------===//

extension Undef {
  static func get(type: Type, _ context: some MutatingContext) -> Undef {
    SILUndef_get(type.bridged, context._bridged).getAs(Undef.self)
  }
}

extension BasicBlock {
  func addBlockArgument(type: Type, ownership: Ownership, _ context: some MutatingContext) -> BlockArgument {
    context.notifyInstructionsChanged()
    return SILBasicBlock_addBlockArgument(bridged, type.bridged, ownership._bridged).blockArgument
  }
  
  func eraseArgument(at index: Int, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    SILBasicBlock_eraseArgument(bridged, index)
  }

  func moveAllInstructions(toBeginOf otherBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    context.notifyBranchesChanged()
    SILBasicBlock_moveAllInstructionsToBegin(bridged, otherBlock.bridged)
  }

  func moveAllInstructions(toEndOf otherBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    context.notifyBranchesChanged()
    SILBasicBlock_moveAllInstructionsToEnd(bridged, otherBlock.bridged)
  }

  func eraseAllArguments(_ context: some MutatingContext) {
    // Arguments are stored in an array. We need to erase in reverse order to avoid quadratic complexity.
    for argIdx in (0 ..< arguments.count).reversed() {
      eraseArgument(at: argIdx, context)
    }
  }

  func moveAllArguments(to otherBlock: BasicBlock, _ context: some MutatingContext) {
    BasicBlock_moveArgumentsTo(bridged, otherBlock.bridged)
  }
}

extension AllocRefInstBase {
  func setIsStackAllocatable(_ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    AllocRefInstBase_setIsStackAllocatable(bridged)
    context.notifyInstructionChanged(self)
  }
}

extension UseList {
  func replaceAll(with replacement: Value, _ context: some MutatingContext) {
    for use in self {
      use.instruction.setOperand(at: use.index, to: replacement, context)
    }
  }
}

extension Instruction {
  func setOperand(at index : Int, to value: Value, _ context: some MutatingContext) {
    if self is FullApplySite && index == ApplyOperands.calleeOperandIndex {
      context.notifyCallsChanged()
    }
    context.notifyInstructionsChanged()
    SILInstruction_setOperand(bridged, index, value.bridged)
    context.notifyInstructionChanged(self)
  }
}

extension RefCountingInst {
  func setAtomicity(isAtomic: Bool, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    RefCountingInst_setIsAtomic(bridged, isAtomic)
    context.notifyInstructionChanged(self)
  }
}

extension TermInst {
  func replaceBranchTarget(from fromBlock: BasicBlock, to toBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyBranchesChanged()
    TermInst_replaceBranchTarget(bridged, fromBlock.bridged, toBlock.bridged)
  }
}

extension Function {
  func set(needStackProtection: Bool, _ context: FunctionPassContext) {
    context.notifyEffectsChanged()
    SILFunction_setNeedStackProtection(bridged, needStackProtection ? 1 : 0)
  }

  func fixStackNesting(_ context: FunctionPassContext) {
    PassContext_fixStackNesting(context._bridged, bridged)
  }

}
