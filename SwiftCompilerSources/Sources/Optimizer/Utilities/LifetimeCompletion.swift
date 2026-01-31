//===--- LifetimeCompletion.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

/// Complete lifetimes which were cut off by an `unreachable` instruction.
///
/// If an optimization creates an `unreachable` this new dead-end in the control flow cuts off
/// liveranges which originally reached over the point of the `unreachable`.
/// In such cases optimizations can restore lifetime ending instructions before the `unreachable`
/// by calling this utility - usually at the end of an optimization pass.
///
/// If, for example, the `end_borrow %2` and `destroy_value %1` has been cut off by an optimization
/// which removes dead code after an `exit` call
/// ```
///   %1 = alloc_ref $C
///   %2 = begin_borrow %1
///   apply @exit()
///   unreachable
/// ```
/// then `completeLifetimes` will restore the lifetime ending instructions
/// ```
///   %1 = alloc_ref $C
///   %2 = begin_borrow %1
///   apply @exit()
///   end_borrow %2
///   destroy_value [dead_end] %1    // The dead_end flag means that this destroy is effectively a no-op
///   unreachable
/// ```
///
/// If `includeTrivialVars` is true then also trivial values which are a result of
/// `move_value [var_decl]` are completed.
///
func completeLifetimes(in function: Function, includeTrivialVars: Bool = false, _ context: FunctionPassContext) {
  context.setNeedCompleteLifetimes(to: false)

  guard function.hasOwnership else {
    return
  }

  let blocks = getDominatingBlocksOfUnreachables(in: function, context)

  // Process blocks in reverse dominance order.
  // This is important to not invalidate nesting of e.g. borrow scopes.
  for block in blocks.reversed() {
    for inst in block.instructions.reversed() {
      for result in inst.results {
        completeLifetime(of: result, includeTrivialVars: includeTrivialVars, context)
      }
    }
    for arg in block.arguments {
      completeLifetime(of: arg, includeTrivialVars: includeTrivialVars, context)
    }
  }
}

/// Completes a the lifetime of a single `value`.
/// See `completeLifetimes(in: Function)`.
///
/// Lifetimes which are completed:
/// * owned values
/// * borrow scopes
/// * `begin_apply` scopes
/// * `store_borrow` scopes
/// * access scopes
/// * trivial values which are a result of `move_value [var_decl]` - if `includeTrivialVars` is true
///
/// The lifetime is complete in blocks where `getDeadEnd` returns an insertion point. The default
/// is to complete lifetimes at `unreachable` instructions.
///
/// This utility can only be used if it's guaranteed that all (potential) outer scopes of `value` are
/// also not completed at the dead-end points - to not invalidate proper nesting of lifetimes.
/// In this example, it's not valid to complete the lifetime of `%2`:
/// ```
///   %1 = alloc_ref $T      // lifetime of %1 is _not_ incomplete at the `unreachable`!
///   %2 = begin_borrow %1
///   destroy_value %1
///   unreachable            // inserting `end_borrow %2` here would be illegal
/// ```
func completeLifetime(of value: Value,
                      includeTrivialVars: Bool = false,
                      getDeadEnd: (BasicBlock) -> Instruction? = { $0.terminator as? UnreachableInst },
                      _ context: FunctionPassContext) {
  switch value.ownership {
  case .owned:
    complete(value: value,
             isEnd: { $0.endsLifetime },
             createEnd: { $0.createDestroyValue(operand: value, isDeadEnd: true) },
             getDeadEnd: getDeadEnd,
             context)
  case .guaranteed:
    if let bf = value as? BorrowedFromInst, bf.borrowedPhi.isReborrow {
      completeBorrowScope(of: bf, getDeadEnd: getDeadEnd, context)
    } else if let beginBorrow = BeginBorrowValue(value) {
      switch beginBorrow {
      case .beginBorrow, .loadBorrow, .dereferenceBorrow:
        completeBorrowScope(of: value, getDeadEnd: getDeadEnd, context)
      case .uncheckOwnershipConversion:
        // `unchecked_ownership_conversion` has this weird requirement that either it has no `end_borrow`s
        // or it must have `end_borrow`s on all paths.
        if !value.uses.endingLifetime.isEmpty {
          completeBorrowScope(of: value, getDeadEnd: getDeadEnd, context)
        }
      case .beginApply(let v):
        if v == (v.definingInstruction as! BeginApplyInst).token {
          completeBorrowScope(of: value, getDeadEnd: getDeadEnd, context)
        }
      case .reborrow:
        // reborrow-phis are handled above via their borrowed-from instructions
        return
      case .functionArgument:
        return
      }
    }
  case .none:
    switch value {
    case let sb as StoreBorrowInst:
      complete(value: sb,
               isEnd: { $0.instruction is EndBorrowInst },
               createEnd: { $0.createEndBorrow(of: sb) },
               getDeadEnd: getDeadEnd,
               context)
    case let mv as MoveValueInst where includeTrivialVars && mv.isFromVarDecl:
      complete(value: mv,
               isEnd: { $0.instruction is ExtendLifetimeInst },
               createEnd: { $0.createExtendLifetime(of: mv) },
               getDeadEnd: getDeadEnd,
               context)
    case let ba as BeginAccessInst:
      complete(value: ba,
               isEnd: { $0.instruction is EndAccessInst },
               createEnd: { $0.createEndAccess(beginAccess: ba) },
               getDeadEnd: getDeadEnd,
               context)
    default:
      return
    }
  case .unowned:
    return
  }
}

/// Complete lifetimes of all `values` at `deadEnds` instructions.
/// Instead of completing the lifetimes at `unreachable` instructions, they are
/// complete at custom `deadEnd` points.
/// The `values` must be in dominance order.
func completeLifetimes(of values: some RandomAccessCollection<Value>,
                       at deadEnds: some RandomAccessCollection<Instruction>,
                       includeTrivialVars: Bool = false,
                       _ context: FunctionPassContext
) {
  var deadEndBlocks = BasicBlockSet(context)
  defer { deadEndBlocks.deinitialize() }
  deadEndBlocks.insert(contentsOf: deadEnds.lazy.map { $0.parentBlock })

  for value in values.reversed() {
    completeLifetime(
      of: value,
      includeTrivialVars: includeTrivialVars,
      getDeadEnd: { block in
        if deadEndBlocks.contains(block) {
          // There are usually very few end instructions. Therefore a linear search is fine.
          return deadEnds.first(where: { $0.parentBlock == block })!
        }
        return nil
      },
      context)
  }
}

private func completeBorrowScope(of value: Value,
                                 getDeadEnd: (BasicBlock) -> Instruction?,
                                 _ context: FunctionPassContext
) {
  complete(value: value,
           isEnd: { $0.endsLifetime },
           createEnd: { $0.createEndBorrow(of: value) },
           getDeadEnd: getDeadEnd,
           context)
}

private func complete(value: Value,
                      isEnd: (Operand) -> Bool,
                      createEnd: (Builder) -> (),
                      getDeadEnd: (BasicBlock) -> Instruction?,
                      _ context: FunctionPassContext
) {
  var endBlocks = BasicBlockSet(context)
  defer { endBlocks.deinitialize() }
  endBlocks.insert(contentsOf: value.uses.lazy.filter(isEnd).map{ $0.instruction.parentBlock })

  var liveBlocks = BasicBlockWorklist(context)
  defer { liveBlocks.deinitialize() }
  liveBlocks.pushIfNotVisited(value.parentBlock)

  // Insert ending instruction at all `unreachable`s which are reachable from `value` without an
  // ending instruction in between.
  while let block = liveBlocks.pop() {
    if endBlocks.contains(block) {
      continue
    }
    if let deadEnd = getDeadEnd(block) {
      let builder = Builder(before: deadEnd, location: deadEnd.location.asAutoGenerated, context)
      createEnd(builder)
    } else {
      liveBlocks.pushIfNotVisited(contentsOf: block.successors)
    }
  }
}

/// Returns all blocks which dominate any dead-end block with an `unreachable`.
/// The returned basic block array is in dominance order.
private func getDominatingBlocksOfUnreachables(in function: Function, _ context: FunctionPassContext) -> [BasicBlock] {
  // We need a valid dominator tree, even if the current pass has changed the control flow.
  context.updateAnalysis()

  let dominatorTree = context.dominatorTree

  var blocksToComplete = BasicBlockSet(context)
  defer { blocksToComplete.deinitialize() }

  for block in function.blocks where block.terminator is UnreachableInst {
    var toInsert = block
    // Walk up the dominator tree and add all parent blocks.
    while blocksToComplete.insert(toInsert) {
      guard let parent = dominatorTree.getImmediateDominator(of: toInsert) else {
        break
      }
      toInsert = parent
    }
  }

  return dominatorTree.getDominanceOrder(startingAt: function.entryBlock) { blocksToComplete.contains($0) }
}

func registerLifetimeCompletion() {
  BridgedOptimizerUtilities.registerLifetimeCompletion(
    // completeAllLifetimes
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction, includeTrivialVars: Bool) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let function = bridgedFunction.function
      completeLifetimes(in: function, includeTrivialVars: includeTrivialVars, context)
    },
    // completeLifetimes
    { (bridgedCtxt: BridgedContext, bridgedValues: BridgedArrayRef, bridgedEnds: BridgedArrayRef) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      bridgedValues.withElements(ofType: BridgedValue.self) { bridgedValueBuffer in
        let values = bridgedValueBuffer.map { $0.value }
        bridgedEnds.withElements(ofType: BridgedInstruction.self) { bridgedInstBuffer in
          let endInsts = bridgedInstBuffer.map { $0.instruction }
          completeLifetimes(of: values, at: endInsts, context)
        }
      }
    }
  )
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let lifetimeComletionTest = FunctionTest("lifetime_completion") {
  function, arguments, context in

  let includeTrivialVars = arguments.hasUntaken ? arguments.takeBool() : false

  completeLifetimes(in: function, includeTrivialVars: includeTrivialVars, context)
}
