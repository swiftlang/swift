//===--- DestroyHoisting.swift ---------------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Hoists `destroy_value` instructions  without shrinking an object's lifetime.
/// This is done if it can be proved that another copy of a value (either in an SSA value or in memory) keeps
/// the referenced object(s) alive until the original position of the `destroy_value`.
///
/// ```
///   %1 = copy_value %0
///   ...
///   last_use_of %0
///   // other instructions
///   destroy_value %0       // %1 is still alive here
/// ```
/// ->
/// ```
///   %1 = copy_value %0
///   ...
///   last_use_of %0
///   destroy_value %0
///   // other instructions
/// ```
///
/// This also works if a copy of the value is kept alive  in memory:
///
/// ```
///   %1 = copy_value %0
///   store %1 to [assign] %a
///   ...
///   last_use_of %0
///   // other instructions
///   destroy_value %0       // memory location %a is not modified since the store
/// ```
/// ->
/// ```
///   %1 = copy_value %0
///   store %0 to [assign] %a
///   ...
///   last_use_of %0
///   destroy_value %0
///   // other instructions
/// ```
///
/// The benefit of this optimization is that it can enable copy-propagation by moving
/// destroys above deinit barries and access scopes.
///
let destroyHoisting = FunctionPass(name: "destroy-hoisting") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }

  for block in function.blocks {
    for arg in block.arguments {
      optimize(value: arg, context)
      if !context.continueWithNextSubpassRun() {
        return
      }
    }
    for inst in block.instructions {
      for result in inst.results {
        optimize(value: result, context)
        if !context.continueWithNextSubpassRun(for: inst) {
          return
        }
      }
    }
  }
}

private func optimize(value: Value, _ context: FunctionPassContext) {
  guard value.ownership == .owned,
        // Avoid all the analysis effort if there are no destroys to hoist.
        !value.uses.filterUsers(ofType: DestroyValueInst.self).isEmpty
  else {
    return
  }

  var (foundDestroys, hoistableDestroys) = selectHoistableDestroys(of: value, context)
  defer { hoistableDestroys.deinitialize() }

  guard foundDestroys else {
    return
  }

  guard var minimalLiverange = InstructionRange(withLiverangeOf: value, ignoring: hoistableDestroys, context) else {
    return
  }
  defer { minimalLiverange.deinitialize() }

  hoistDestroys(of: value, toEndOf: minimalLiverange, restrictingTo: &hoistableDestroys, context)
}

private func selectHoistableDestroys(of value: Value, _ context: FunctionPassContext) -> (Bool, InstructionSet) {
  // Also includes liveranges of copied values and values stored to memory.
  var forwardExtendedLiverange = InstructionRange(withForwardExtendedLiverangeOf: value, context)
  defer { forwardExtendedLiverange.deinitialize() }

  let deadEndBlocks = context.deadEndBlocks
  var foundDestroys = false
  var hoistableDestroys = InstructionSet(context)

  for use in value.uses {
    if let destroy = use.instruction as? DestroyValueInst,
       // We can hoist all destroys for which another copy of the value is alive at the destroy.
       forwardExtendedLiverange.contains(destroy),
       // TODO: once we have complete OSSA lifetimes we don't need to handle dead-end blocks.
       !deadEndBlocks.isDeadEnd(destroy.parentBlock)
    {
      foundDestroys = true
      hoistableDestroys.insert(destroy)
    }
  }
  return (foundDestroys, hoistableDestroys)
}

private func hoistDestroys(of value: Value,
                           toEndOf minimalLiverange: InstructionRange,
                           restrictingTo hoistableDestroys: inout InstructionSet,
                           _ context: FunctionPassContext)
{
  createNewDestroys(for: value, atEndPointsOf: minimalLiverange, reusing: &hoistableDestroys, context)

  createNewDestroys(for: value, atExitPointsOf: minimalLiverange, reusing: &hoistableDestroys, context)

  removeDestroys(of: value, restrictingTo: hoistableDestroys, context)
}

private func createNewDestroys(
  for value: Value,
  atEndPointsOf liverange: InstructionRange,
  reusing hoistableDestroys: inout InstructionSet,
  _ context: FunctionPassContext
) {
  let deadEndBlocks = context.deadEndBlocks

  for endInst in liverange.ends {
    if !endInst.endsLifetime(of: value) {
      Builder.insert(after: endInst, context) { builder in
        builder.createDestroy(of: value, reusing: &hoistableDestroys, notIn: deadEndBlocks)
      }
    }
  }
}

private func createNewDestroys(
  for value: Value,
  atExitPointsOf liverange: InstructionRange,
  reusing hoistableDestroys: inout InstructionSet,
  _ context: FunctionPassContext
) {
  let deadEndBlocks = context.deadEndBlocks

  for exitBlock in liverange.exitBlocks {
    let builder = Builder(atBeginOf: exitBlock, context)
    builder.createDestroy(of: value, reusing: &hoistableDestroys, notIn: deadEndBlocks)
  }
}

private func removeDestroys(
  of value: Value,
  restrictingTo hoistableDestroys: InstructionSet,
  _ context: FunctionPassContext
) {
  for use in value.uses {
    if let destroy = use.instruction as? DestroyValueInst,
       hoistableDestroys.contains(destroy)
    {
      context.erase(instruction: destroy)
    }
  }
}

private extension InstructionRange {

  init?(withLiverangeOf initialDef: Value, ignoring ignoreDestroys: InstructionSet, _ context: FunctionPassContext)
  {
    var liverange = InstructionRange(for: initialDef, context)
    var visitor = InteriorUseWalker(definingValue: initialDef, ignoreEscape: false, visitInnerUses: true, context) {
      if !ignoreDestroys.contains($0.instruction) {
        liverange.insert($0.instruction)
      }
      return .continueWalk
    }
    defer { visitor.deinitialize() }

    // This is important to visit begin_borrows which don't have an end_borrow in dead-end blocks.
    // TODO: we can remove this once we have complete lifetimes.
    visitor.innerScopeHandler = {
      if let inst = $0.definingInstruction {
        liverange.insert(inst)
      }
      return .continueWalk
    }

    guard visitor.visitUses() == .continueWalk else {
      liverange.deinitialize()
      return nil
    }
    self = liverange
  }

  // In addition to the forward-extended liverange, also follows copy_value's transitively.
  init(withForwardExtendedLiverangeOf initialDef: Value, _ context: FunctionPassContext) {
    self.init(for: initialDef, context)

    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(initialDef)
    while let value = worklist.pop() {
      assert(value.ownership == .owned)

      for use in value.uses {
        let user = use.instruction
        if !use.endsLifetime {
          if let copy = user as? CopyValueInst {
            worklist.pushIfNotVisited(copy)
          }
          continue
        }

        switch user {
        case let store as StoreInst:
          extendLiverangeInMemory(of: initialDef, with: store, context)

        case let termInst as TermInst & ForwardingInstruction:
          worklist.pushIfNotVisited(contentsOf: termInst.forwardedResults.lazy.filter({ $0.ownership != .none }))

        case is ForwardingInstruction, is MoveValueInst:
          if let result = user.results.lazy.filter({ $0.ownership != .none }).singleElement {
            worklist.pushIfNotVisited(result)
          }

        default:
          // We cannot extend a lexical liverange with a non-lexical liverange, because afterwards the
          // non-lexical liverange could be shrunk over a deinit barrier which would let the original
          // lexical liverange to be shrunk, too.
          if !initialDef.isInLexicalLiverange(context) || value.isInLexicalLiverange(context) {
            self.insert(user)
          }
        }
      }
    }
  }

  private mutating func extendLiverangeInMemory(
    of initialDef: Value,
    with store: StoreInst,
    _ context: FunctionPassContext
  ) {
    let domTree = context.dominatorTree

    if initialDef.destroyUsers(dominatedBy: store.parentBlock, domTree).isEmpty {
      return
    }

    // We have to take care of lexical lifetimes. See comment above.
    if initialDef.isInLexicalLiverange(context) &&
       !store.destination.accessBase.isInLexicalOrGlobalLiverange(context)
    {
      return
    }

    if isTakeOrDestroy(ofAddress: store.destination, after: store, beforeDestroysOf: initialDef, context) {
      return
    }

    self.insert(contentsOf: initialDef.destroyUsers(dominatedBy: store.parentBlock, domTree).map { $0.next! })
  }
}

private func isTakeOrDestroy(
  ofAddress address: Value,
  after store: StoreInst,
  beforeDestroysOf initialDef: Value,
  _ context: FunctionPassContext
) -> Bool {
  let aliasAnalysis = context.aliasAnalysis
  let domTree = context.dominatorTree
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(store.next!)
  while let inst = worklist.pop() {
    if inst.endsLifetime(of: initialDef) {
      continue
    }
    if inst.mayTakeOrDestroy(address: address, aliasAnalysis) {
      return true
    }
    if let next = inst.next {
      worklist.pushIfNotVisited(next)
    } else {
      for succ in inst.parentBlock.successors where store.parentBlock.dominates(succ, domTree) {
        worklist.pushIfNotVisited(succ.instructions.first!)
      }
    }
  }
  return false
}

private extension Builder {
  func createDestroy(of value: Value,
                     reusing hoistableDestroys: inout InstructionSet,
                     notIn deadEndBlocks: DeadEndBlocksAnalysis) {
    guard case .before(let insertionPoint) = insertionPoint else {
      fatalError("unexpected kind of insertion point")
    }
    if deadEndBlocks.isDeadEnd(insertionPoint.parentBlock) {
      return
    }
    if hoistableDestroys.contains(insertionPoint) {
      hoistableDestroys.erase(insertionPoint)
    } else {
      createDestroyValue(operand: value)
    }
  }
}

private extension Value {
  func destroyUsers(dominatedBy domBlock: BasicBlock, _ domTree: DominatorTree) ->
        LazyMapSequence<LazyFilterSequence<LazyMapSequence<UseList, DestroyValueInst?>>, DestroyValueInst> {
    return uses.lazy.compactMap { use in
      if let destroy = use.instruction as? DestroyValueInst,
         domBlock.dominates(destroy.parentBlock, domTree)
      {
         return destroy
      }
      return nil
    }
  }
}

private extension Instruction {
  func endsLifetime(of value: Value) -> Bool {
    return operands.contains { $0.value == value && $0.endsLifetime }
  }

  func mayTakeOrDestroy(address: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    switch self {
    case is BeginAccessInst, is EndAccessInst, is EndBorrowInst:
      return false
    default:
      return mayWrite(toAddress: address, aliasAnalysis)
    }
  }
}

private extension AccessBase {
  func isInLexicalOrGlobalLiverange(_ context: FunctionPassContext) -> Bool {
    switch self {
    case .box(let pbi):      return pbi.box.isInLexicalLiverange(context)
    case .class(let rea):    return rea.instance.isInLexicalLiverange(context)
    case .tail(let rta):     return rta.instance.isInLexicalLiverange(context)
    case .stack(let asi):    return asi.isLexical
    case .global:            return true
    case .argument(let arg):
      switch arg.convention {
      case .indirectIn, .indirectInGuaranteed, .indirectInout, .indirectInoutAliasable:
        return arg.isLexical
      default:
        return false
      }
    case .yield, .storeBorrow, .pointer, .index, .unidentified:
      return false
    }
  }
}
