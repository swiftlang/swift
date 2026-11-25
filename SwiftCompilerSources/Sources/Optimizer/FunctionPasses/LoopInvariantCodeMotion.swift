//===--- LoopInvariantCodeMotion.swift ------------------------------------===//
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

private func log(_ message: @autoclosure () -> String) {
  llvmDebug("loop-invariant-code-motion", message())
}

/// Hoist loop invariant code out of innermost loops.
let loopInvariantCodeMotionPass = FunctionPass(name: "loop-invariant-code-motion") { function, context in
  for loop in context.loopTree.loops {
    optimizeTopLevelLoop(topLevelLoop: loop, context)
  }
  
  if context.needFixStackNesting {
    context.fixStackNesting(in: function)
  }
}

private func optimizeTopLevelLoop(topLevelLoop: Loop, _ context: FunctionPassContext) {
  var innerLoops = Stack<Loop>(context)
  defer { innerLoops.deinitialize() }
  
  getWorkList(forLoop: topLevelLoop, workList: &innerLoops)

  while let thisLoop = innerLoops.pop() {
    // We only support Loops with a preheader.
    guard thisLoop.preheader != nil else {
      continue
    }
    
    var thisLoopChanged = false

    repeat {
      var movableInstructions = analyzeLoopAndSplitLoads(loop: thisLoop, context)
      thisLoopChanged = optimizeLoop(loop: thisLoop, movableInstructions: &movableInstructions, context)
    } while thisLoopChanged
  }
}

/// Creates post-order DFS work list of inner loops.
private func getWorkList(forLoop loop: Loop, workList: inout Stack<Loop>) {
  workList.push(loop)
  
  for innerLoop in loop.innerLoops {
    getWorkList(forLoop: innerLoop, workList: &workList)
  }
}

/// Instructions that can be moved outside the loop.
private struct MovableInstructions {
  var loadAndStoreAccessPaths: [AccessPath] = []
  
  var speculativelyHoistable: [Instruction] = []
  var loadsAndStores: [Instruction] = []
  var hoistUp: [Instruction] = []
  var scopedInsts: [ScopedInstruction] = []
  var stackAllocations: [AllocStackInst] = []
}

/// Analyzed instructions inside the currently processed loop.
private struct AnalyzedInstructions {
  /// Side effects of the loop.
  var loopSideEffects: StackWithCount<Instruction>
  /// Non-load instructions that have no side effects but may read from memory.
  var loopReadOnlyEffects: Stack<Instruction>
  var loads: Stack<LoadInst>
  var loadBorrows: Stack<LoadBorrowInst>
  var stores: Stack<StoreInst>
  /// Stack allocations which, or a projection of which, are copied into a stack allocation in the loop.
  var copiedStackAllocations: InstructionSet

  // loop blocks that dominate all exiting and latch blocks
  var dominatingBlocks: BasicBlockSet

  /// `true` if the loop has instructions which (may) read from memory, which are not in `loads`,
  /// `loadBorrows` and not in `sideEffects`.
  var hasOtherMemReadingInsts: Bool {
    !loopReadOnlyEffects.isEmpty
  }
  
  init (in loop: Loop, _ context: FunctionPassContext) {
    self.loopSideEffects = StackWithCount<Instruction>(context)
    self.loopReadOnlyEffects = Stack<Instruction>(context)
    self.loads = Stack<LoadInst>(context)
    self.loadBorrows = Stack<LoadBorrowInst>(context)
    self.stores = Stack<StoreInst>(context)
    self.copiedStackAllocations = InstructionSet(context)
    self.dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(context)
  }
  
  mutating func deinitialize() {
    loopSideEffects.deinitialize()
    loopReadOnlyEffects.deinitialize()
    loads.deinitialize()
    loadBorrows.deinitialize()
    stores.deinitialize()
    copiedStackAllocations.deinitialize()
    dominatingBlocks.deinitialize()
  }
}

/// Analyzes `loop` for hosting/sinking potential.
/// Computes `MovableInstructions` we may be able to move out of the loop
///
/// This may split some loads into smaller loads.
private func analyzeLoopAndSplitLoads(loop: Loop, _ context: FunctionPassContext) -> MovableInstructions {
  var movableInstructions = MovableInstructions()
  var analyzedInstructions = AnalyzedInstructions(in: loop, context)
  defer { analyzedInstructions.deinitialize() }

  analyzeInstructions(in: loop, &analyzedInstructions, context)

  collectProjectableAccessPathsAndSplitLoads(in: loop, &analyzedInstructions, &movableInstructions, context)

  collectSpeculativelyMovableInstructions(in: loop, analyzedInstructions, &movableInstructions, context)

  collectMovableInstructions(in: loop, analyzedInstructions, &movableInstructions, context)
    
  return movableInstructions
}

/// Analyze instructions inside the `loop`. Compute side effects and populate `analyzedInstructions`.
///
private func analyzeInstructions(
  in loop: Loop,
  _ analyzedInstructions: inout AnalyzedInstructions,
  _ context: FunctionPassContext
) {
  for bb in loop.loopBlocks {
    for inst in bb.instructions {
      switch inst {
      case is FixLifetimeInst:
        continue // We can ignore the side effects of FixLifetimes
      case let loadInst as LoadInst:
        analyzedInstructions.loads.append(loadInst)
        continue // Don't set `hasOtherMemReadingInsts`
      case let loadBorrowInst as LoadBorrowInst:
        analyzedInstructions.loadBorrows.append(loadBorrowInst)
        continue // Don't set `hasOtherMemReadingInsts`
      case let storeInst as StoreInst:
        if let load = storeInst.source as? LoadInst, load.uses.isSingleUse, load.parentBlock == storeInst.parentBlock {
          analyzedInstructions.recordCopiedStackAllocation(of: storeInst, in: loop)
        }
        analyzedInstructions.stores.append(storeInst)
      case let copyAddrInst as CopyAddrInst:
        analyzedInstructions.recordCopiedStackAllocation(of: copyAddrInst, in: loop)
      default:
        break
      }
      if inst.mayHaveSideEffects {
        analyzedInstructions.loopSideEffects.append(inst)
      } else if inst.mayReadFromMemory {
        analyzedInstructions.loopReadOnlyEffects.append(inst)
      }
    }
  }
}

/// Collect memory locations for which we can move all loads and stores out of the loop.
/// `loads` may mutate during this loop.
private func collectProjectableAccessPathsAndSplitLoads(
  in loop: Loop,
  _ analyzedInstructions: inout AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  if !analyzedInstructions.hasOtherMemReadingInsts {
    for storeInst in analyzedInstructions.stores {
      let accessPath = storeInst.destination.accessPath
      if accessPath.isLoopInvariant(loop: loop),
         analyzedInstructions.isOnlyLoadedAndStored(
           accessPath: accessPath,
           storeAddr: storeInst.destination,
           in: loop,
           context.aliasAnalysis
         ),
         !movableInstructions.loadAndStoreAccessPaths.contains(accessPath),
         // This is not a requirement for functional correctness, but we don't want to
         // _speculatively_ load and store the value (outside of the loop).
         analyzedInstructions.storesCommonlyDominateExits(of: loop, storingTo: accessPath, context),
         analyzedInstructions.splitLoads(
           storeAddr: storeInst.destination,
           accessPath: accessPath,
           context
         ) {
        movableInstructions.loadAndStoreAccessPaths.append(accessPath)
      }
    }
  }
}

/// Collect movable instructions, even if they are not executed in every loop iteration.
private func collectSpeculativelyMovableInstructions(in loop: Loop,
                                                     _ analyzedInstructions: AnalyzedInstructions,
                                                     _ movableInstructions: inout MovableInstructions,
                                                     _ context: FunctionPassContext) {

  var storeCounter = 0

  for bb in loop.loopBlocks {
    for inst in bb.instructions {
      switch inst {
      case is LoadInst, is LoadBorrowInst:
        movableInstructions.loadsAndStores.append(inst)
      case let storeInst as StoreInst:
        // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
        if storeCounter * analyzedInstructions.loopSideEffects.count < 8000 {
          storeCounter += 1
          // `hoistAndSinkLoadsAndStores` would erase a store after `speculativelyHoistInstructions` hoisted it.
          if collectSpeculativelyHoistableStoreInstruction(in: loop, storeInst: storeInst, analyzedInstructions, &movableInstructions, context) {
            break
          }
        }
        movableInstructions.loadsAndStores.append(inst)

      case let refElementAddrInst as RefElementAddrInst:
        movableInstructions.speculativelyHoistable.append(refElementAddrInst)
      default:
        break
      }
    }
  }
}

/// Collect a `store` instruction as speculatively hoistable if appropriate. If
/// its destination is (part of) a stack allocation inside the loop, also
/// potentially hoist that. Returns true if the store is collected.
private func collectSpeculativelyHoistableStoreInstruction(
  in loop: Loop,
  storeInst: StoreInst,
  _ analyzedInstructions: AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) -> Bool {

  // Only attempt to hoist trivial stores for now. Hoisting an init or assign
  // store could have visible side effects depending on the type's inits and
  // deinit.
  if storeInst.storeOwnership != .trivial {
    return false
  }

  // If the stored value is produced inside the loop, we cannot hoist the store.
  if loop.contains(block: storeInst.source.parentBlock) {
    return false
  }

  let accessPath = storeInst.destination.accessPath

  log("Processing candidate hoistable store:\n\(storeInst.description)")

  // Even a trivial address can be projected out of a non-trivial one.
  // Hoisting the store in this case could hoist it into a region
  // where the base address is borrowed. Bail out to avoid this when
  // the accessBase is not known to be trivial.
  guard let baseAddress = accessPath.base.address
  else {
    log("Access base is unknown. Cannot hoist.")
    return false
  }

  guard baseAddress.type.isTrivial(in: storeInst.parentFunction)
  else {
    log("Access base is non-trivial. Cannot hoist.\n\(baseAddress).")
    return false
  }

  if !storesUniqueReadableValue(
    analyzedInstructions: analyzedInstructions,
    accessPath: accessPath, storeInst: storeInst, context)
  {
    log("Found an instruction with a read or write effect that prevents store hoisting.")
    return false
  }

  // We can only hoist the store if doing so would have no observable effects on
  // the rest of the function. There are three main cases to consider:
  // - an alloc_stack inside the loop,
  // - an alloc_stack outside the loop or any other kind of address.
  //
  // An alloc_stack inside the loop is inaccessible after it ends, so hoisting
  // can have no observable effects, though we must also hoist the alloc_stack.
  //
  // In other cases, the address lives after the end of the loop, so we can only
  // hoist the store if the address is guaranteed to contain the stored value at
  // loop exit. This is true if and only if the store dominates all exits. I.e.
  // We can only hoist the store non-speculatively.
  //
  // In the following example, we cannot hoist the store because it is in a
  // conditionally-executed branch.
  //
  //   var n = 5 // Local or global
  //
  //   for i in 0..<10 {
  //     if pred(i) {
  //       n = 6
  //     }
  //   }
  //
  //   print(n) // 5 or 6?
  if case .stack(let allocStackInst) = accessPath.base,
    loop.contains(block: allocStackInst.parentBlock)
  {
    log("Store writes to an alloc_stack inside the loop.\n\(allocStackInst)")
    // speculatively hoistable instructions are hoisted before any other
    // instructions, so we can only hoist the store and the alloc_stack together
    // if the store writes directly to the alloc_stack.
    if storeInst.destination != allocStackInst {
      log("Store destination is not the alloc_stack. Cannot hoist.\n\(storeInst.destination)")
      return false
    }
    log("Hoisting alloc_stack and store.")
    movableInstructions.speculativelyHoistable.append(allocStackInst)
    movableInstructions.speculativelyHoistable.append(storeInst)
    return true
  }

  if !analyzedInstructions.dominatingBlocks.contains(storeInst.parentBlock) {
    log("Store writes to an address outside the loop. Not safe to hoist speculatively.")
    return false
  }

  log("Store dominates all loop exits and latches. Safe to hoist.")
  movableInstructions.speculativelyHoistable.append(storeInst)
  return true
}


/// Returns true only if `storeInst` dominates all reads that may alias with the
/// provided path, and there are no potentially aliasing writes that may
/// overwrite the value `storeInst` stored.
///
/// For efficiency, the implementation may not detect all such cases, for
/// example, with stores in branches.
private func storesUniqueReadableValue(
  analyzedInstructions: AnalyzedInstructions,
  accessPath: AccessPath, storeInst: StoreInst, _ context: FunctionPassContext
) -> Bool {
  precondition(storeInst.storeOwnership == .trivial)
  let aliasAnalysis = context.aliasAnalysis
  let domTree = context.dominatorTree
  let storeAddr = storeInst.destination

  /// If `inst` has a write effect on `storeAddr`, or it has a read effect and
  /// is not dominated by `storeInst`, it has a conflicting effect. In this
  /// case we know `storeInst` does not meet our conditions.
  let hasConflictingEffect = { (inst: Instruction) in
    log(inst.description)
    // Ignore the store itself.

    switch inst {
    case storeInst:
      log("   instruction does not conflict with itself")
      return false
    case is DeallocStackInst:
      // dealloc_stack of a trivial type ignores the actual written value so
      // we can ignore its memory effects
      log("   ignoring the effects of dealloc_stack")
      return false
    default:
      break
    }

    // Pass the original address value until we can fix alias analysis.
    let effect = aliasAnalysis.getMemoryEffect(of: inst, on: storeAddr)
    if effect.write {
      log("    CONFLICTS: write effect")
      return true
    } else if effect.read && !storeInst.dominates(inst, domTree) {
      log("    CONFLICTS: read effect and instruction is not dominated by the store")
      return true
    }

    log("    no conflicting read or write effect")
    return false
  }

  // loopSideEffects, loopReadOnlyEffects, loads and loadBorrows partition the set of
  // instructions with memory effects in the loop.
  if analyzedInstructions.loopSideEffects.contains(where: hasConflictingEffect) {
    return false
  }
  if analyzedInstructions.loopReadOnlyEffects.contains(where: hasConflictingEffect) {
    return false
  }
  if analyzedInstructions.loads.contains(where: hasConflictingEffect) {
    return false
  }
  if analyzedInstructions.loadBorrows.contains(where: hasConflictingEffect) {
    return false
  }

  return true
}

/// Collect movable instructions. Only includes instructions which are executed in every loop iteration.
private func collectMovableInstructions(
  in loop: Loop,
  _ analyzedInstructions: AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  var loadInstCounter = 0
  var readOnlyApplyCounter = 0
  for bb in loop.loopBlocks {
    // Skip blocks which are not executed in every loop iteration
    guard analyzedInstructions.dominatingBlocks.contains(bb) else {
      continue
    }

    for inst in bb.instructions {
      switch inst {
      case let loadInst as LoadInst:
        // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
        if loadInstCounter * analyzedInstructions.loopSideEffects.count < 8000,
           !analyzedInstructions.sideEffectsMayWrite(to: loadInst.address, context.aliasAnalysis),

           // Usually `load [take]` is not hoisted anyway, because there must be another instruction in the
           // loop which re-initializes the loaded memory location.
           // However, this is not necessarily true for alloc_stack locations with dynamic lifetime, e.g. a
           // pack-loop which is specialized for a single pack element (at runtime the loop is only executed once).
           loadInst.loadOwnership != .take
        {
          movableInstructions.hoistUp.append(loadInst)
        }
        
        loadInstCounter += 1
      case is UncheckedOwnershipConversionInst:
        break // TODO: Add support
      case is StoreInst:
          // Stores are moved out of the loop in `hoistAndSinkLoadsAndStores` and `speculativelyHoistInstructions`.
          break
      case let condFailInst as CondFailInst:
        // We can (and must) hoist cond_fail instructions if the operand is
        // invariant. We must hoist them so that we preserve memory safety. A
        // cond_fail that would have protected (executed before) a memory access
        // must - after hoisting - also be executed before said access.
        movableInstructions.hoistUp.append(condFailInst)
      case let beginAccessInst as BeginAccessInst:
        if beginAccessInst.canScopedInstructionBeHoisted(outOf: loop, analyzedInstructions: analyzedInstructions, context) {
          movableInstructions.scopedInsts.append(beginAccessInst)
        }
      case let beginBorrowInst as BeginBorrowInstruction:
        if !beginBorrowInst.isLexical && beginBorrowInst.canScopedInstructionBeHoisted(outOf: loop, analyzedInstructions: analyzedInstructions, context) {
          movableInstructions.scopedInsts.append(beginBorrowInst)
        }
      case let allocStackInst as AllocStackInst:
        // An allocation in a loop never carries a value into the next iteration. Hoisting it only pays off by keeping
        // it alive over the uses of a copy's destination, which lets TempRValueElimination remove the copy.
        if analyzedInstructions.copiedStackAllocations.contains(allocStackInst) {
          movableInstructions.stackAllocations.append(allocStackInst)
        } else {
          log("Not hoisting stack allocation \(allocStackInst)")
        }
      case let fullApplySite as FullApplySite:
        // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
        if readOnlyApplyCounter * analyzedInstructions.loopSideEffects.count < 8000,
           fullApplySite.isSafeReadOnlyApply(for: analyzedInstructions.loopSideEffects, in: loop, context)
        {
          if let beginApplyInst = fullApplySite as? BeginApplyInst {
            movableInstructions.scopedInsts.append(beginApplyInst)
          } else {
            movableInstructions.hoistUp.append(fullApplySite)
          }
          readOnlyApplyCounter += 1
        } else if let callee = fullApplySite.referencedFunction,
                  callee.isGlobalInitFunction,
                  !fullApplySite.globalInitMayConflictWith(analyzedInstructions.loopSideEffects, context) {
          movableInstructions.hoistUp.append(fullApplySite)
        }
      case let builtin as BuiltinInst where builtin.id == .Once || builtin.id == .OnceWithContext:
        if !builtin.globalInitMayConflictWith(analyzedInstructions.loopSideEffects, context) {
          movableInstructions.hoistUp.append(builtin)
        }
      default:
        if inst.canBeHoisted(outOf: loop, context) {
          movableInstructions.hoistUp.append(inst)
        }
      }
    }
  }
}

/// Optimizes the loop by performing in following order:
/// - speculative hoist
/// - projection, hoist and sink of loads and stores
/// - hoist of instructions that are guaranteed to be executed
/// - sink
/// - hoist with sink of scoped instructions
/// - hoist of stack allocations, if nothing else moved
private func optimizeLoop(
  loop: Loop,
  movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) -> Bool {
  var changed = false
  
  // TODO: If we hoist tuple_element_addr and struct_element_addr instructions here, hoistAndSinkLoadsAndStores could converge after just one execution!
  changed = movableInstructions.speculativelyHoistInstructions(outOf: loop, context)  || changed
  changed = movableInstructions.hoistAndSinkLoadsAndStores(outOf: loop, context)      || changed
  changed = movableInstructions.hoistInstructions(outOf: loop, context)               || changed
  changed = movableInstructions.hoistWithSinkScopedInstructions(outOf: loop, context) || changed

  // Speculative store hoisting in `speculativelyHoistInstructions` can only hoist a conditional store to an `alloc_stack` that is still inside the loop.
  // That's why we can hoist the remaining stack allocations only after the previous steps have converged.
  if !changed {
    changed = movableInstructions.hoistStackAllocations(outOf: loop, context)
  }

  return changed
}

extension BasicBlock {
  func containsStoresTo(accessPath: AccessPath) -> Bool {
    return instructions.contains { inst in
      (inst as? StoreInst)?.destination.accessPath == accessPath
    }
  }
}

private extension AnalyzedInstructions {
  /// Returns true if all instructions in `sideEffects` which may alias with
  /// this path are either loads or stores from this path.
  ///
  /// `storeAddr` is only needed for AliasAnalysis until we have an interface
  /// that supports `AccessPath`.
  func isOnlyLoadedAndStored(
    accessPath: AccessPath,
    storeAddr: Value,
    in loop: Loop,
    _ aliasAnalysis: AliasAnalysis
  ) -> Bool {
    if loopSideEffects.contains(where: { sideEffect in
      // Alias analysis reports an `end_borrow` of a `load_borrow` as writing the loaded memory location
      // (to prevent the memory from being modified during the borrow scope). Ignore it if its `load_borrow`
      // is replaced by a borrow of the stored value in `hoistAndSinkLoadAndStore`, because then the
      // `end_borrow` doesn't refer to the memory location anymore.
      // An `end_borrow` of a non-replaceable `load_borrow` is a conflict, which is checked below.
      if sideEffect.isPartOfReplaceableLoadBorrow(of: accessPath, in: loop) {
        return false
      }
      switch sideEffect {
      case let storeInst as StoreInst:
        if storeInst.storesTo(accessPath) {
          // TODO: Add support for `store [assign]`. Moving such a store out of the loop
          //       requires to insert a `destroy_value` for the overwritten value at the
          //       original store location.
          return storeInst.storeOwnership == .assign
        }
      case let loadInst as LoadInst:
        if loadInst.loadsFrom(accessPath) {
          return false
        }
      default: break
      }

      // Pass the original address value until we can fix alias analysis.
      return sideEffect.mayReadOrWrite(address: storeAddr, aliasAnalysis)
    }) {
      return false
    }

    if loads.contains(where: { loadInst in
      loadInst.mayRead(fromAddress: storeAddr, aliasAnalysis) && !loadInst.overlaps(accessPath: accessPath)
    }) {
      return false
    }

    // A `load_borrow` which may read from the memory location must be replaced by a borrow of the
    // stored value in `hoistAndSinkLoadAndStore`, because after the transformation the memory location
    // doesn't contain a valid value inside the loop.
    // In contrast to a `load`, which is split if it's wider than `accessPath` (see `splitLoads`), a
    // `load_borrow` can only be replaced if it loads from `accessPath` or a projection of it, and if its
    // borrow scope ends within the loop (see `isReplaceable`).
    if loadBorrows.contains(where: { loadBorrow in
      loadBorrow.mayRead(fromAddress: storeAddr, aliasAnalysis) && !loadBorrow.isReplaceable(by: accessPath, in: loop)
    }) {
      return false
    }

    if stores.contains(where: { storeInst in
      storeInst.mayWrite(toAddress: storeAddr, aliasAnalysis) && !storeInst.storesTo(accessPath)
    }) {
      return false
    }
    
    return true
  }

  /// Returns `true` if all stores to `accessPath` commonly dominate the loop exits.
  func storesCommonlyDominateExits(of loop: Loop, storingTo accessPath: AccessPath, _ context: FunctionPassContext) -> Bool {
    var exitingBlocksSet = BasicBlockSet(context)
    var storeBlocks = BasicBlockSet(context)
    var worklist = BasicBlockWorklist(context)
    defer {
      exitingBlocksSet.deinitialize()
      storeBlocks.deinitialize()
      worklist.deinitialize()
    }
    
    // Also a store in the pre-header dominates all exists. Although the situation
    // is a bit different here: the store in the pre-header remains - it's not
    // (re)moved by the LICM transformation.
    // But even if the loop-stores are not dominating the loop exits, it
    // makes sense to move them out of the loop if this case. When this is done,
    // dead-store-elimination can then most likely eliminate the store in the
    // pre-header.
    //
    //   pre_header:
    //     store %v1 to %addr
    //   header:
    //     cond_br %cond, then, tail
    //   then:
    //     store %v2 to %addr    // a conditional store in the loop
    //     br tail
    //   tail:
    //     cond_br %loop_cond, header, exit
    //   exit:
    //
    //  will be transformed to
    //
    //   pre_header:
    //     store %v1 to %addr    // <- can be removed by DSE afterwards
    //   header:
    //     cond_br %cond, then, tail
    //   then:
    //     br tail
    //   tail(%phi):
    //     cond_br %loop_cond, header, exit
    //   exit:
    //     store %phi to %addr
    //
    if loop.preheader!.containsStoresTo(accessPath: accessPath) {
      return true
    }
    
    // Create a set of exiting blocks for efficient lookup later.
    exitingBlocksSet.insert(contentsOf: loop.exitingBlocks)
    
    // Collect as many recognizable store parent blocks as possible. It's ok if not all stores are collected.
    storeBlocks.insert(contentsOf: stores
      .filter({ $0.destination.accessPath == accessPath })
      .map(\.parentBlock))
    
    // If a store is in the loop header, we already know that it's dominating all loop exits.
    if storeBlocks.contains(loop.header) {
      return true
    }
    
    // Starting from the header, check whether all stores are alive.
    worklist.pushIfNotVisited(loop.header)
    while let block = worklist.pop() {
      if storeBlocks.contains(block) {
        continue
      }
      
      if exitingBlocksSet.contains(block),
         !block.successors.allSatisfy({ $0.terminator is UnreachableInst }) {
        return false
      }
      
      worklist.pushIfNotVisited(contentsOf: block.successors)
    }
    
    return true
  }
  
  /// Returns true if `loopSideEffects` contains any memory writes which
  /// may alias with the memory `address`.
  func sideEffectsMayWrite(to address: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    return loopSideEffects
      .contains { sideEffect in
        sideEffect.mayWrite(toAddress: address, aliasAnalysis)
      }
  }

  /// Returns true if `loopSideEffects` contains any memory writes outside of `scope` which may alias
  /// with `address`.
  ///
  /// Alias analysis reports an `end_borrow` of a `load_borrow` as writing the loaded memory location.
  /// But it doesn't really write to memory. It only prevents that the memory location is changed during
  /// the borrow scope. When `scope` is hoisted out of the loop and its end is sunk out of the loop, all
  /// `load_borrow` scopes in the loop are completely inside the extended scope (see `isEndBorrowOfLoadBorrow`).
  /// Therefore it's correct to ignore all `end_borrow`s of `load_borrow`s.
  ///
  /// However, `ignoreEndBorrowsOfOtherLoadBorrows` is only true if this is also beneficial:
  /// * true: for hoisting a `begin_access [read]`. Extending a read access scope over other borrow scopes
  ///   has no negative effect.
  /// * false: for hoisting a `load_borrow`. Then only `end_borrow`s of `load_borrow`s which are inside `scope`
  ///   (i.e. the `end_borrow` of the hoisted `load_borrow` itself) are ignored. Hoisting a `load_borrow`
  ///   over the borrow scopes of other `load_borrow`s of the same memory location would create a borrow
  ///   scope which spans the whole loop. Such a scope prevents other optimizations.
  func sideEffectsMayWrite(to address: Value,
                           outsideOf scope: InstructionRange,
                           ignoreEndBorrowsOfOtherLoadBorrows: Bool,
                           _ context: FunctionPassContext
  ) -> Bool {
    for sideEffectInst in loopSideEffects {
      if sideEffectInst.mayWrite(toAddress: address, context.aliasAnalysis),
         !scope.inclusiveRangeContains(sideEffectInst),
         !(ignoreEndBorrowsOfOtherLoadBorrows ? sideEffectInst.isEndBorrowOfLoadBorrow
                                              : scope.containsBorrowScopeBegin(of: sideEffectInst))
      {
        return true
      }
    }
    return false
  }

  func sideEffectsMayReadOrWrite(to address: Value,
                                 outsideOf scope: InstructionRange,
                                 _ context: FunctionPassContext
  ) -> Bool {
    for sideEffectInst in loopSideEffects {
      if sideEffectInst.mayReadOrWrite(address: address, context.aliasAnalysis),
         !scope.inclusiveRangeContains(sideEffectInst),
         !sideEffectInst.isEndBorrowOfLoadBorrow
      {
        return true
      }
    }
    return false
  }

  /// Find all loads that contain `accessPath`. Split them into a load with
  /// identical `accessPath` and a set of non-overlapping loads. Add the new
  /// non-overlapping loads to `loads`.
  mutating func splitLoads(
    storeAddr: Value,
    accessPath: AccessPath,
    _ context: FunctionPassContext
  ) -> Bool {
    var newLoads = Stack<LoadInst>(context)
    defer {
      loads.append(contentsOf: newLoads)
      newLoads.deinitialize()
    }
    var splitCounter = 0

    while let loadInst = loads.pop() {
      // Found a load wider than the store to accessPath.
      //
      // SplitLoads is called for each unique access path in the loop that is
      // only loaded from and stored to and this loop takes time proportional to:
      //   num-wide-loads x num-fields x num-loop-memops
      //
      // For each load wider than the store, it creates a new load for each field
      // in that type. Each new load is inserted in the LoadsAndStores vector.  To
      // avoid super-linear behavior for large types (e.g. giant tuples), limit
      // growth of new loads to an arbitrary constant factor per access path.
      guard splitCounter <= 6 else {
        newLoads.push(loadInst)
        return false
      }

      guard !loadInst.isDeleted, loadInst.address.accessPath.contains(accessPath) else {
        newLoads.push(loadInst)
        continue
      }

      guard let projectionPath =  loadInst.address.accessPath.getProjection(to: accessPath),
            let splitLoads = loadInst.trySplit(alongPath: projectionPath, context) else {
        newLoads.push(loadInst)
        return false
      }
      
      splitCounter += splitLoads.count
      newLoads.append(contentsOf: splitLoads)
    }

    return true
  }

  mutating func recordCopiedStackAllocation(of copy: CopyLikeInstruction, in loop: Loop) {
    if copy.isInitializationOfDestination,
       let destination = copy.destinationAddress as? AllocStackInst,
       loop.contains(block: destination.parentBlock),
       case .stack(let source) = copy.sourceAddress.accessBase
    {
      copiedStackAllocations.insert(source)
    }
  }
}

private extension MovableInstructions {
  /// Hoist instructions speculatively.
  ///
  /// Contrary to `hoistInstructions`, it doesn't only go through instructions in blocks that dominate all exits.
  mutating func speculativelyHoistInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false
    
    for inst in speculativelyHoistable {
      changed = inst.hoist(outOf: loop, context) || changed
    }
    
    return changed
  }
  
  /// Hoists and sinks loads with matching stores. Projects loads.
  mutating func hoistAndSinkLoadsAndStores(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false
    
    for accessPath in loadAndStoreAccessPaths {
      changed = hoistAndSinkLoadAndStore(outOf: loop, accessPath: accessPath, context: context) || changed
    }
    
    return changed
  }

  /// Only hoists instructions in blocks that dominate all exit and latch blocks.
  /// It doesn't hoist instructions speculatively.
  mutating func hoistInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false

    for hoistableInst in hoistUp {
      changed = hoistableInst.hoist(outOf: loop, context) || changed
    }

    return changed
  }

  /// Hoist and sink scoped instructions.
  mutating func hoistWithSinkScopedInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    // For infinite loops we could end up with partially hoisted scoped instruction that could
    // lead to e.g. value overconsumption.
    if loop.hasNoExitBlocks {
      return false
    }
    
    var changed = false

    for scopedInst in scopedInsts {
      if let storeBorrowInst = scopedInst as? StoreBorrowInst,
         loop.contains(block: storeBorrowInst.allocStack.parentBlock) 
      {
        guard storeBorrowInst.allocStack.hoist(outOf: loop, context) else {
          continue
        }
      }

      guard scopedInst.hoist(outOf: loop, context) else {
        continue
      }

      // We only want to sink the first end_access and erase the rest to not introduce duplicates.
      var sankFirst = false
      for endAccess in scopedInst.endInstructions {
        if sankFirst {
          context.erase(instruction: endAccess)
        } else {
          sankFirst = endAccess.sink(outOf: loop, context)
        }
      }
      
      changed = true
    }

    return changed
  }

  /// Hoist stack allocations and sink their deallocations.
  mutating func hoistStackAllocations(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false

    for allocStackInst in stackAllocations {
      changed = allocStackInst.hoist(outOf: loop, context) || changed
    }

    return changed
  }

  private mutating func hoistAndSinkLoadAndStore(
    outOf loop: Loop,
    accessPath: AccessPath,
    context: FunctionPassContext
  ) -> Bool {

    // If the memory is not initialized at all exits, it would be wrong to insert stores at exit blocks.
    guard memoryIsInitializedAtAllExits(of: loop, accessPath: accessPath, context) else {
      return false
    }

    // Initially load the value in the loop pre header.
    let builder = Builder(before: loop.preheader!.terminator, context)
    var firstStore: StoreInst?
    
    // If there are multiple stores in a block, only the last one counts.
    for case let storeInst as StoreInst in loadsAndStores where storeInst.storesTo(accessPath) {
      // If a store just stores the loaded value, bail. The operand (= the load)
      // will be removed later, so it cannot be used as available value.
      // This corner case is surprisingly hard to handle, so we just give up.
      if let srcLoadInst = storeInst.source as? LoadInst,
         srcLoadInst.loadsFrom(accessPath) {
        return false
      }
      
      if firstStore == nil {
        firstStore = storeInst
      } else if storeInst.destination.type != firstStore!.destination.type {
        // This transformation assumes that the values of all stores in the loop
        // must be interchangeable. It won't work if stores different types
        // because of casting or payload extraction even though they have the
        // same access path.
        return false
      }
    }
    
    guard let firstStore else {
      return false
    }

    let function = firstStore.parentFunction

    guard allSplitLoadTakesAreOfSameField(of: accessPath) else {
      return false
    }

    let phiOwnership: Ownership =
      function.hasOwnership && !firstStore.source.type.isTrivial(in: function) ? .owned : .none

    var ssaUpdater = InstructionBasedSSAUpdater(
      type: firstStore.destination.type.objectType,
      ownership: phiOwnership,
      context
    )
    defer { ssaUpdater.deinitialize() }

    // Set all stored values as available values in the ssaUpdater.
    for case let storeInst as StoreInst in loadsAndStores where storeInst.storesTo(accessPath) {
      ssaUpdater.addAvailableValue(storeInst.source, after: storeInst)
    }

    var cloner = Cloner(cloneBefore: loop.preheader!.terminator, context)
    defer { cloner.deinitialize() }
    
    guard let initialAddr = (cloner.cloneRecursively(value: firstStore.destination) { srcAddr, cloner in
      switch srcAddr {
      case is AllocStackInst, is BeginBorrowInst, is MarkDependenceInst:
        return .stopCloning
      default: break
      }
      
      // Clone projections until the address dominates preheader.
      if srcAddr.parentBlock.dominates(loop.preheader!, context.dominatorTree) {
        cloner.recordFoldedValue(srcAddr, mappedTo: srcAddr)
        return .customValue(srcAddr)
      } else {
        // Return nil invalid to continue cloning.
        return .defaultValue
      }
    }) else {
      return false
    }

    let ownership: LoadInst.LoadOwnership
    if !function.hasOwnership {
      ownership = .unqualified
    } else if initialAddr.type.isTrivial(in: function) {
      ownership = .trivial
    } else {
      ownership = .take
    }

    let initialLoad = builder.createLoad(fromAddress: initialAddr, ownership: ownership)
    ssaUpdater.addAvailableValue(initialLoad, after: initialLoad)

    // A `load [take]` is replaced by destructuring the current value and re-constructing it with
    // an `undef` for the taken field. The re-constructed value is available after the `load [take]`.
    // For a `load [take]` of the whole value, the re-constructed value is just `undef`.
    // As it is created later, use a placeholder for now.
    var loadTakePlaceholders = Dictionary<LoadInst, MarkDependenceInst>()
    for case let load as LoadInst in loadsAndStores where load.loadOwnership == .take && load.loadsFrom(accessPath) {
      let builder = Builder(after: load, context)
      let undef = Undef.get(type: firstStore.destination.type.objectType, context)
      let placeholder = builder.createMarkDependence(value: undef, base: undef, ownership: phiOwnership,
                                                     kind: .Unresolved)
      loadTakePlaceholders[load] = placeholder
      ssaUpdater.addAvailableValue(placeholder, after: placeholder)
    }
    
    var changed = false
    
    // Replace the loads with the current value.
    for inst in loadsAndStores {
      // All `load_borrow`s which load from `accessPath` are replaceable, which is checked in `isOnlyLoadedAndStored`.
      if let loadBorrow = inst as? LoadBorrowInst,
         let projectionPath = accessPath.getProjection(to: loadBorrow.address.accessPath)
      {
        let rootVal = ssaUpdater.getValue(before: loadBorrow)
        loadBorrow.replace(withBorrowOf: rootVal, projectedAlong: projectionPath, context)
        changed = true
        continue
      }
      
      guard let loadInst = inst as? LoadInst,
            loadInst.loadsFrom(accessPath) else {
        continue
      }
      
      let rootVal = ssaUpdater.getValue(before: loadInst)

      if let placeholder = loadTakePlaceholders[loadInst] {
        let projectionPath = accessPath.getProjection(to: loadInst.address.accessPath)!
        let builder = Builder(before: loadInst, context)
        let undef = Undef.get(type: loadInst.type, context)
        let (projected, updated) = rootVal.createProjectionAndReplace(with: undef, path: projectionPath,
                                                                      builder: builder)
        placeholder.valueOperand.set(to: updated, context)
        loadInst.replace(with: projected, context)
        changed = true
        continue
      }

      if loadInst.address.accessPath == accessPath {
        if loadInst.loadOwnership == .copy {
          let builder = Builder(before: loadInst, context)
          let copy = builder.createCopyValue(operand: rootVal)
          loadInst.replace(with: copy, context)
        } else {
          loadInst.replace(with: rootVal, context)
        }
        changed = true
        continue
      }
      
      guard let projectionPath = accessPath.getProjection(to: loadInst.address.accessPath) else {
        continue
      }
    
      let builder = Builder(before: loadInst, context)
      let projection = if loadInst.loadOwnership == .copy || loadInst.loadOwnership == .trivial {
        rootVal.createProjectionAndCopy(path: projectionPath, builder: builder)
      } else {
        rootVal.createProjection(path: projectionPath, builder: builder)
      }
      loadInst.replace(with: projection, context)
      
      changed = true
    }

    for case let load as LoadInst in loadsAndStores {
      if let placeholder = loadTakePlaceholders[load] {
        placeholder.replace(with: placeholder.value, context)
      }
    }

    // The re-constructed value of a `load [take]` has no ownership, because the only non-trivial field
    // is `undef` (or the whole value is `undef`). But inserted phis are created with "owned" ownership.
    // Such phis would never be consumed, because the value is not stored back.
    canonicalizeOwnershipToNone(of: ssaUpdater.insertedPhis, context)

    // The stores are only removed at the end, because they are needed as insertion points for available
    // values in the ssaUpdater.
    for case let storeInst as StoreInst in loadsAndStores where storeInst.storesTo(accessPath) {
      context.erase(instruction: storeInst)
      changed = true
    }
    
    loadsAndStores.removeAll(where: { $0.isDeleted })
    
    // Store back the value at all loop exits.
    for exitBlock in loop.exitBlocks {
      assert(exitBlock.hasSinglePredecessor, "Exiting edge should not be critical.")
      
      let builder = Builder(before: exitBlock.instructions.first!, context)
      
      builder.createStore(
        source: ssaUpdater.getValue(atBeginOf: exitBlock),
        destination: initialAddr,
        ownership: firstStore.storeOwnership
      )
      changed = true
    }
    
    // In case the value is only stored but never loaded in the loop.
    if initialLoad.uses.isEmpty {
      context.erase(instruction: initialLoad)
    }
    
    return changed
  }

  /// Returns true if all split `load [take]`s of `accessPath` (i.e. `load [take]`s of a projected address)
  /// take the same field.
  ///
  /// For simplicity, only support splitting `load [take]`s of the same field.
  /// With this restriction, a `load [take]` can never destructure a value which contains an `undef` field,
  /// because in valid OSSA the taken field must be re-initialized by a store before it can be taken again.
  ///
  /// This restriction also guarantees that a split `load [take]` is never inside the borrow scope of a
  /// `load_borrow` of another field. This would result in a lifetime end of the borrowed value inside
  /// the borrow scope:
  /// ```
  ///   %b = load_borrow %fieldB                      %bb = begin_borrow %value
  ///                                         ->      %b = struct_extract %bb, #fieldB
  ///   %a = load [take] %fieldA                      (%a, ...) = destructure_struct %value  // lifetime end of %value!
  ///   end_borrow %b                                 end_borrow %bb
  /// ```
  /// In valid OSSA, the non-trivial field B must be de-initialized before the next store of the whole value.
  /// This can only be done by a `load [take]` of B, which is a split `load [take]` of a different field,
  /// or by an instruction which writes to B, e.g. `destroy_addr`, which bails in `isOnlyLoadedAndStored`.
  func allSplitLoadTakesAreOfSameField(of accessPath: AccessPath) -> Bool {
    var firstSplitLoadTakePath: AccessPath? = nil
    for case let load as LoadInst in loadsAndStores where load.isSplitLoadTake(of: accessPath) {
      let path = load.address.accessPath
      if let firstSplitLoadTakePath {
        if path != firstSplitLoadTakePath {
          return false
        }
      } else {
        firstSplitLoadTakePath = path
      }
    }
    return true
  }

  func memoryIsInitializedAtAllExits(of loop: Loop, accessPath: AccessPath, _ context: FunctionPassContext) -> Bool {

    // Perform a simple dataflow analysis which checks if there is a path from a `load [take]`
    // (= the only kind of instruction which can de-initialize the memory) to a loop exit without
    // a `store` in between.

    var stores = InstructionSet(context)
    defer { stores.deinitialize() }
    for case let store as StoreInst in loadsAndStores where store.storesTo(accessPath) {
      stores.insert(store)
    }

    var exitInsts = InstructionSet(context)
    defer { exitInsts.deinitialize() }
    exitInsts.insert(contentsOf: loop.exitBlocks.lazy.map { $0.instructions.first! })

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    for case let load as LoadInst in loadsAndStores where load.loadOwnership == .take && load.loadsFrom(accessPath) {
      worklist.pushIfNotVisited(load)
    }

    while let inst = worklist.pop() {
      if stores.contains(inst) {
        continue
      }
      if exitInsts.contains(inst) {
        return false
      }
      worklist.pushSuccessors(of: inst)
    }
    return true
  }
}

private func hoistAllocAndDealloc(
  allocStackInst: AllocStackInst, outOf loop: Loop, _ context: FunctionPassContext
) -> Bool {
  guard loop.contains(block: allocStackInst.parentBlock), !loop.hasNoExitBlocks else {
    return false
  }

  var liveRange = BasicBlockRange(begin: allocStackInst.parentBlock, context)
  defer { liveRange.deinitialize() }
  liveRange.insert(contentsOf: allocStackInst.deallocations.map { $0.parentBlock })

  let deallocationsInLoop = allocStackInst.deallocations.filter { loop.contains(block: $0.parentBlock) }

  log("Hoisting \(allocStackInst)")
  allocStackInst.move(before: loop.preheader!.terminator, context)

  for deallocStack in deallocationsInLoop {
    context.erase(instruction: deallocStack)
  }

  for exitBlock in loop.exitBlocks where !liveRange.contains(exitBlock.singlePredecessor!) {
    let builder = Builder(before: exitBlock.instructions.first!, context)
    builder.createDeallocStack(allocStackInst)
  }

  context.notifyInvalidatedStackNesting()

  return true
}

private extension Instruction {
  /// Returns `true` if this instruction follows the default hoisting heuristic which means it
  /// is not a terminator, allocation or deallocation and either a hoistable array semantics call or doesn't have memory effects.
  func canBeHoisted(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    switch self {
    case is TermInst, is Allocation, is Deallocation:
      return false
    default:
      break
    }

    if memoryEffects == .noEffects,
       !results.contains(where: { $0.ownership == .owned }) {
      return true
    }

    return false
  }
  
  func hoist(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    guard operands.allSatisfy({ !loop.contains(block: $0.value.parentBlock) }) else {
      return false
    }

    let terminator = loop.preheader!.terminator
    if canHoistArraySemanticsCall(to: terminator, context) {
      hoistArraySemanticsCall(before: terminator, context)
    } else {
      if let loadCopyInst = self as? LoadInst, loadCopyInst.loadOwnership == .copy {
        hoist(loadCopyInst: loadCopyInst, outOf: loop, context)
        return true
      } else if let allocStackInst = self as? AllocStackInst {
        return hoistAllocAndDealloc(allocStackInst: allocStackInst, outOf: loop, context)
      } else {
        move(before: terminator, context)
      }
    }
    return true
  }
  
  private func hoist(loadCopyInst: LoadInst, outOf loop: Loop, _ context: FunctionPassContext) {
    if loop.hasNoExitBlocks {
      return
    }
    
    let preheaderBuilder = Builder(before: loop.preheader!.terminator, context)
    let preheaderLoadBorrow = preheaderBuilder.createLoadBorrow(fromAddress: loadCopyInst.address)
    
    let headerBuilder = Builder(before: loadCopyInst, context)
    let copyValue = headerBuilder.createCopyValue(operand: preheaderLoadBorrow)
    loadCopyInst.replace(with: copyValue, context)
    
    for exitBlock in loop.exitBlocks {
      assert(exitBlock.hasSinglePredecessor, "Exiting edge should not be critical.")
      
      let exitBlockBuilder = Builder(before: exitBlock.instructions.first!, context)
      exitBlockBuilder.createEndBorrow(of: preheaderLoadBorrow)
    }
  }

  func sink(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false

    for exitBlock in loop.exitBlocks {
      assert(exitBlock.hasSinglePredecessor, "Exiting edge should not be critical.")
      
      if changed {
        copy(before: exitBlock.instructions.first!, context)
      } else {
        move(before: exitBlock.instructions.first!, context)
        changed = true
      }
    }

    return changed
  }
  
  /// Returns `true` if `sideEffect` cannot be reordered with a call to this
  /// global initializer.
  private func globalInitMayConflictWith(
    sideEffect: Instruction,
    _ aliasAnalysis: AliasAnalysis
  ) -> Bool {
    switch sideEffect {
    case let storeInst as StoreInst:
      return mayReadOrWrite(address: storeInst.destinationOperand.value, aliasAnalysis)
    case let loadInst as LoadInst:
      return mayWrite(toAddress: loadInst.address, aliasAnalysis)
    case is CondFailInst:
      return false
    default:
      return true
    }
  }
  
  /// Returns `true` if any of the instructions in `loopSideEffects` which are
  /// post-dominated by a call to this global initializer cannot be reordered with
  /// the call.
  func globalInitMayConflictWith(
    _ loopSideEffects: StackWithCount<Instruction>,
    _ context: FunctionPassContext
  ) -> Bool {
    let aliasAnalysis = context.aliasAnalysis
    let postDominatorTree = context.postDominatorTree
    return loopSideEffects
      .contains { sideEffect in
        // Only check instructions in blocks which are "before" (i.e. post-dominated
        // by) the block which contains the init-call.
        self.strictlyPostDominates(sideEffect, postDominatorTree) &&
        globalInitMayConflictWith(sideEffect: sideEffect, aliasAnalysis)
      }
  }
}

private extension StoreInst {
  /// Returns a `true` if this store is a store to `accessPath`.
  func storesTo(_ accessPath: AccessPath) -> Bool {
    return accessPath == self.destination.accessPath
  }
}

private extension LoadInst {
  /// Returns `true` if this is a `load [take]` from a projected address of `accessPath`.
  func isSplitLoadTake(of accessPath: AccessPath) -> Bool {
    loadOwnership == .take && address.accessPath != accessPath && loadsFrom(accessPath)
  }

  /// Returns `true` if this load instruction loads from `accessPath` or a
  /// projected address from `accessPath`.
  func loadsFrom(_ accessPath: AccessPath) -> Bool {
    return accessPath.getProjection(to: self.address.accessPath)?.isMaterializable ?? false
  }
  
  func overlaps(accessPath: AccessPath) -> Bool {
    if let path = accessPath.getProjection(to: self.address.accessPath),
       // If the accessPath is wider than load, it needs to be materializable.
       // Otherwise we won't be able to project it.
       path.isMaterializable {
      // The load is narrower than the access path.
      return true
    }
    
    if self.address.accessPath.isEqualOrContains(accessPath) {
      // The load is wider than the access path.
      return true
    }
    
    return false
  }
}

/// Changes the ownership of phis to `.none` if none of the incoming values has ownership.
private func canonicalizeOwnershipToNone(of phis: [Phi], _ context: FunctionPassContext) {
  var changed = true
  while changed {
    changed = false
    for phi in phis where phi.value.ownership == .owned {
      if phi.incomingValues.allSatisfy({ $0.ownership == .none }) {
        phi.value.set(ownership: .none, context)
        changed = true
      }
    }
  }
}

private extension LoadBorrowInst {

  /// Returns `true` if this `load_borrow` loads from `accessPath` or a projected address from `accessPath`
  /// and can be replaced by a borrow of the stored value in `hoistAndSinkLoadAndStore`.
  func isReplaceable(by accessPath: AccessPath, in loop: Loop) -> Bool {
    guard let projectionPath = accessPath.getProjection(to: address.accessPath),
          projectionPath.isMaterializable
    else {
      return false
    }
    // The borrow scope must end within the loop, because the stored value is consumed by the
    // `store` which is inserted at the begin of the loop exit blocks.
    // Reborrows are not supported.
    return scopeEndingOperands.allSatisfy { $0.instruction is EndBorrowInst && loop.contains(block: $0.instruction.parentBlock) }
  }

  /// Replaces this `load_borrow` with a borrow of `value`, projected along `path`.
  func replace(withBorrowOf value: Value, projectedAlong path: SmallProjectionPath, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    let endBorrowOperands = Array(scopeEndingOperands)
    if value.ownership == .owned {
      let borrow = builder.createBeginBorrow(of: value)
      for endBorrowOp in endBorrowOperands {
        endBorrowOp.set(to: borrow, context)
      }
      replace(with: borrow.createProjection(path: path, builder: builder), context)
    } else {
      for endBorrowOp in endBorrowOperands {
        context.erase(instruction: endBorrowOp.instruction)
      }
      replace(with: value.createProjection(path: path, builder: builder), context)
    }
  }
}

private extension Instruction {
  /// Returns `true` if this is a `load_borrow` or its `end_borrow` and the `load_borrow` can be
  /// replaced by the stored value in `hoistAndSinkLoadAndStore`.
  func isPartOfReplaceableLoadBorrow(of accessPath: AccessPath, in loop: Loop) -> Bool {
    switch self {
    case let loadBorrow as LoadBorrowInst:
      return loadBorrow.isReplaceable(by: accessPath, in: loop)
    case let endBorrow as EndBorrowInst:
      if let loadBorrow = endBorrow.borrow as? LoadBorrowInst {
        return loadBorrow.isReplaceable(by: accessPath, in: loop)
      }
      return false
    default:
      return false
    }
  }
}

private extension FullApplySite {
  /// Returns `true` if the `sideEffects` contain any memory writes which
  /// may alias with any memory which is read by this `ApplyInst`.
  /// - Note: This function should only be called on a read-only apply!
  func isSafeReadOnlyApply(
    for sideEffects: StackWithCount<Instruction>,
    in loop: Loop,
    _ context: FunctionPassContext
  ) -> Bool {
    switch arraySemanticsCallKind {
    case .getCount, .getCapacity:
      if canHoistArraySemanticsCall(to: loop.preheader!.terminator, context) {
        return true
      }
    default:
      break
    }
    guard functionConvention.resultsWithError.allSatisfy({ $0.convention == .unowned }) else {
      return false
    }
    if let callee = referencedFunction,
       callee.hasSemanticsAttribute("array.props.isNativeTypeChecked")
    {
      return false
    }
    if context.calleeAnalysis.getSideEffects(ofApply: self).memory == .noEffects {
      return true
    }

    // Check if the memory addressed by the argument may alias any writes.
    for sideEffect in sideEffects {
      switch sideEffect {
      case let storeInst as StoreInst:
        if storeInst.storeOwnership == .assign ||
           mayRead(fromAddress: storeInst.destination, context.aliasAnalysis) {
          return false
        }
      case let copyAddrInst as CopyAddrInst:
        if !copyAddrInst.isInitializationOfDestination ||
           mayRead(fromAddress: copyAddrInst.destination, context.aliasAnalysis) {
          return false
        }
      case let fullApplySite as FullApplySite:
        if context.calleeAnalysis.getSideEffects(ofApply: fullApplySite).memory.write {
          return false
        }
      case is CondFailInst, is StrongRetainInst, is UnmanagedRetainValueInst,
        is RetainValueInst, is StrongRetainUnownedInst, is FixLifetimeInst,
        is KeyPathInst, is DeallocStackInst, is DeallocStackRefInst,
        is DeallocRefInst:
        break
      case let endApply as EndApplyInst:
        if endApply.beginApply != self {
          return false
        }
      default:
        if sideEffect.mayWriteToMemory {
          return false
        }
      }
    }

    return true
  }
}

private extension InstructionRange {
  /// Returns true if `inst` is an `end_borrow` whose `load_borrow` is inside this range.
  func containsBorrowScopeBegin(of inst: Instruction) -> Bool {
    if let endBorrow = inst as? EndBorrowInst,
       let loadBorrow = endBorrow.borrow as? LoadBorrowInst
    {
      return inclusiveRangeContains(loadBorrow)
    }
    return false
  }
}

private extension Instruction {
  /// Returns true if this is an `end_borrow` of a `load_borrow`.
  ///
  /// Alias analysis reports an `end_borrow` of a `load_borrow` as writing the loaded memory location,
  /// so that the memory location cannot be changed during the borrow scope. If a scope is hoisted out of
  /// the loop (and its end is sunk out of the loop), it's fine to also include such an `end_borrow`
  /// because the whole borrow scope is then inside the extended scope: in valid OSSA, an `end_borrow`
  /// inside a loop cannot end a borrow scope which begins outside the loop. Otherwise there would be
  /// a path which ends the borrow scope twice.
  /// For example, a `load_borrow` within a `begin_access` scope whose `end_borrow` is after the `end_access`:
  /// ```
  ///   %1 = begin_access [read] %0
  ///   %2 = load_borrow %1
  ///   end_access %1
  ///   end_borrow %2
  /// ```
  /// Or a `load_borrow` within a static access scope which precedes a hoisted `modify` access:
  /// ```
  ///   %1 = begin_access [read] [static] %0
  ///   %2 = load_borrow %1
  ///   end_borrow %2
  ///   end_access %1
  ///   %3 = begin_access [modify] [dynamic] %0   // hoisted out of the loop
  /// ```
  var isEndBorrowOfLoadBorrow: Bool {
    if let endBorrow = self as? EndBorrowInst {
      return endBorrow.borrow is LoadBorrowInst
    }
    return false
  }
}

private extension ScopedInstruction {
  /// Returns `true` if this begin access is safe to hoist.
  func canScopedInstructionBeHoisted(
    outOf loop: Loop,
    analyzedInstructions: AnalyzedInstructions,
    _ context: FunctionPassContext
  ) -> Bool {
    guard endInstructions.allSatisfy({ loop.contains(block: $0.parentBlock) && !($0 is TermInst) }) else {
      return false
    }

    var scope = InstructionRange(begin: self, ends: endInstructions, context)
    defer { scope.deinitialize() }
    
    // Instruction specific range related conditions
    switch self {
    case is BeginApplyInst:
      return true // Has already been checked with other full applies.
    case let loadBorrowInst as LoadBorrowInst:
      // Don't hoist a `load_borrow` over `end_borrow`s of other `load_borrow`s of the same memory location.
      // This would be correct, but it creates a borrow scope which spans the whole loop. Such a scope
      // prevents RedundantLoadElimination from eliminating the memory location, e.g. an `alloc_stack`
      // of a sequence iterator which is only borrowed in the loop.
      return !analyzedInstructions.sideEffectsMayWrite(to: loadBorrowInst.address, outsideOf: scope,
                                                       ignoreEndBorrowsOfOtherLoadBorrows: false, context)

    case let beginAccess as BeginAccessInst:
      if beginAccess.accessKind == .read {
        // Check that we don't generate nested accesses when extending the access scope. Also, we must not
        // extend a "read" access scope over a memory write (to the same address) even if the write is _not_
        // in an access scope, because this would confuse alias analysis.
        return !analyzedInstructions.sideEffectsMayWrite(to: beginAccess.address, outsideOf: scope,
                                                         ignoreEndBorrowsOfOtherLoadBorrows: true, context)
      } else {
        // This does not include memory-reading instructions which are not in `loopSideEffects`, like a
        // plain `load`. This is fine because we can extend a "modify" access scope over memory reads
        // (of the same address) as long as we are not generating nested accesses.
        return !analyzedInstructions.sideEffectsMayReadOrWrite(to: beginAccess.address, outsideOf: scope, context)
      }
    case is BeginBorrowInst, is StoreBorrowInst:
      // Ensure the value is produced outside the loop.
      return !loop.contains(block: operands.first!.value.parentBlock)
    default:
      return false
    }
  }
}

private extension AccessPath {
  /// Returns `true` if this access path is invariant in `loop`.
  func isLoopInvariant(loop: Loop) -> Bool {
    switch base {
    case .box(let inst as Instruction), .class(let inst as Instruction),
      .index(let inst as Instruction),
      .pointer(let inst as Instruction), .stack(let inst as Instruction),
      .storeBorrow(let inst as Instruction), .borrowMutateAccessor(let inst as Instruction),
      .tail(let inst as Instruction):
      if loop.contains(block: inst.parentBlock) {
        return false
      }
    case .global, .argument:
      break
    case .yield(let beginApplyResult):
      if loop.contains(block: beginApplyResult.parentBlock) {
        return false
      }
    case .unidentified:
      return false
    }

    return projectionPath.isConstant
  }
}
