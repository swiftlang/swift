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

  // For very large functions this optimization can run into noticeable quadratic
  // behavior. Therefore, ignore functions with more than 100000 SIL instructions.
  // This limit is large enough to not affect most of real-world SIL functions.
  if function.instructions.countExceeds(100000) {
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
        !value.uses.filter(usersOfType: DestroyValueInst.self).isEmpty
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

  placeDestroys(of: value, atBoundaryOf: minimalLiverange, reusing: &hoistableDestroys, context)
}

private func selectHoistableDestroys(of value: Value, _ context: FunctionPassContext) -> (Bool, InstructionSet) {
  // Also includes liveranges of copied values and values stored to memory.
  var forwardExtendedLiverange = InstructionRange(withForwardExtendedLiverangeOf: value, context)
  defer { forwardExtendedLiverange.deinitialize() }

  var foundDestroys = false
  var hoistableDestroys = InstructionSet(context)

  for use in value.uses {
    if let destroy = use.instruction as? DestroyValueInst,
       // We can hoist all destroys for which another copy of the value is alive at the destroy.
       forwardExtendedLiverange.contains(destroy)
    {
      foundDestroys = true
      hoistableDestroys.insert(destroy)
    }
  }
  return (foundDestroys, hoistableDestroys)
}

/// Places the scope-ending instructions of `value` at the boundary of `liverange`.
///
/// Ends move in either direction: when `liverange` reaches beyond the existing ends they sink.
/// `isEnd` recognizes an existing end among `value`'s uses and `createEnd` emits a new one at the
/// builder's insertion point; parameterizing both is what lets this place `end_access` as well as
/// `destroy_value`. Ends in `reusableEnds` are kept where the boundary already falls on them and
/// erased otherwise.
internal func placeScopeEnds(of value: Value,
                             atBoundaryOf liverange: InstructionRange,
                             reusing reusableEnds: inout InstructionSet,
                             isEnd: (Operand) -> Bool,
                             createEnd: (Builder) -> (),
                             _ context: FunctionPassContext)
{
  createEnds(for: value, atEndPointsOf: liverange, reusing: &reusableEnds,
             isEnd: isEnd, createEnd: createEnd, context)

  createEnds(for: value, atExitPointsOf: liverange, reusing: &reusableEnds,
             createEnd: createEnd, context)

  removeEnds(of: value, restrictingTo: reusableEnds, isEnd: isEnd, context)
}

/// Places `destroy_value`s of the owned `value` at the boundary of `liverange`.
internal func placeDestroys(of value: Value,
                            atBoundaryOf liverange: InstructionRange,
                            reusing reusableDestroys: inout InstructionSet,
                            _ context: FunctionPassContext)
{
  let deadEndBlocks = context.deadEndBlocks
  placeScopeEnds(of: value, atBoundaryOf: liverange, reusing: &reusableDestroys,
                 isEnd: { $0.endsLifetime },
                 createEnd: {
                   $0.createDestroyValue(operand: value,
                                         isDeadEnd: deadEndBlocks.isDeadEnd($0.insertionBlock!))
                 },
                 context)
}

private func createEnds(
  for value: Value,
  atEndPointsOf liverange: InstructionRange,
  reusing reusableEnds: inout InstructionSet,
  isEnd: (Operand) -> Bool,
  createEnd: (Builder) -> (),
  _ context: FunctionPassContext
) {
  for endInst in liverange.ends {
    if endInst.operands.contains(where: { $0.value == value && isEnd($0) }) {
      // The boundary already falls on an end of `value`. Keep that one instead of adding a
      // duplicate next to it, and don't let `removeEnds` erase it.
      reusableEnds.erase(endInst)
      continue
    }
    Builder.insert(after: endInst, context) { builder in
      builder.createEnd(reusing: &reusableEnds, createEnd)
    }
  }
}

private func createEnds(
  for value: Value,
  atExitPointsOf liverange: InstructionRange,
  reusing reusableEnds: inout InstructionSet,
  createEnd: (Builder) -> (),
  _ context: FunctionPassContext
) {
  for exitBlock in liverange.exitBlocks {
    let builder = Builder(atBeginOf: exitBlock, context)
    builder.createEnd(reusing: &reusableEnds, createEnd)
  }
}

private func removeEnds(
  of value: Value,
  restrictingTo reusableEnds: InstructionSet,
  isEnd: (Operand) -> Bool,
  _ context: FunctionPassContext
) {
  for use in value.uses where isEnd(use) && reusableEnds.contains(use.instruction) {
    context.erase(instruction: use.instruction)
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
          self.insert(user)
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
  func createEnd(reusing reusableEnds: inout InstructionSet, _ createEnd: (Builder) -> ()) {
    guard case .before(let insertionPoint) = insertionPoint else {
      fatalError("unexpected kind of insertion point")
    }
    if reusableEnds.contains(insertionPoint) {
      reusableEnds.erase(insertionPoint)
    } else {
      createEnd(self)
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
  func mayTakeOrDestroy(address: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    switch self {
    case is BeginAccessInst, is EndAccessInst, is EndBorrowInst:
      return false
    default:
      return mayWrite(toAddress: address, aliasAnalysis)
    }
  }
}
