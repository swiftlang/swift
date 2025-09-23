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
  
  var immediatelyHoistable: [Instruction] = []
  var loadsAndStores: [Instruction] = []
  var hoistUp: [Instruction] = []
  var sinkDown: [Instruction] = []
  var scopedInsts: [ScopedInstruction] = []
}

/// Analyzed instructions inside the currently processed loop.
private struct AnalyzedInstructions {
  /// Side effects of the loop.
  var loopSideEffects: StackWithCount<Instruction>
  
  private var blockSideEffectBottomMarker: StackWithCount<Instruction>.Marker
  
  /// Side effects of the currently analyzed block.
  var sideEffectsOfCurrentBlock: StackWithCount<Instruction>.Segment {
    return StackWithCount<Instruction>.Segment(
      in: loopSideEffects,
      low: blockSideEffectBottomMarker,
      high: loopSideEffects.top
    )
  }
  
  /// Contains either:
  /// * an apply to the addressor of the global
  /// * a builtin "once" of the global initializer
  var globalInitCalls: Stack<Instruction>
  var readOnlyApplies: Stack<FullApplySite>
  var loads: Stack<LoadInst>
  var stores: Stack<StoreInst>
  var scopedInsts: Stack<UnaryInstruction>
  var fullApplies: Stack<FullApplySite>
  
  /// `true` if the loop has instructions which (may) read from memory, which are not in `Loads` and not in `sideEffects`.
  var hasOtherMemReadingInsts = false
  
  /// `true` if one of the side effects might release.
  lazy var sideEffectsMayRelease = loopSideEffects.contains(where: { $0.mayRelease })
  
  init (_ context: FunctionPassContext) {
    self.loopSideEffects = StackWithCount<Instruction>(context)
    self.blockSideEffectBottomMarker = loopSideEffects.top
    
    self.globalInitCalls = Stack<Instruction>(context)
    self.readOnlyApplies = Stack<FullApplySite>(context)
    self.loads = Stack<LoadInst>(context)
    self.stores = Stack<StoreInst>(context)
    self.scopedInsts = Stack<UnaryInstruction>(context)
    self.fullApplies = Stack<FullApplySite>(context)
  }
  
  mutating func deinitialize() {
    readOnlyApplies.deinitialize()
    globalInitCalls.deinitialize()
    loopSideEffects.deinitialize()
    loads.deinitialize()
    stores.deinitialize()
    scopedInsts.deinitialize()
    fullApplies.deinitialize()
  }
  
  /// Mark the start of currently processed block side effects.
  mutating func markBeginOfBlock() {
    blockSideEffectBottomMarker = loopSideEffects.top
  }
}

/// Analyzes `loop` for hosting/sinking potential.
/// Computes `MovableInstructions` we may be able to move out of the loop
///
/// This may split some loads into smaller loads.
private func analyzeLoopAndSplitLoads(loop: Loop, _ context: FunctionPassContext) -> MovableInstructions {
  // TODO: Remove once uses lowered OSSA.
  loop.splitCriticalExitingAndBackEdges(context)

  var movableInstructions = MovableInstructions()
  var analyzedInstructions = AnalyzedInstructions(context)
  defer { analyzedInstructions.deinitialize() }

  analyzeInstructions(in: loop, &analyzedInstructions, &movableInstructions, context)

  collectProjectableAccessPathsAndSplitLoads(in: loop, &analyzedInstructions, &movableInstructions, context)
  
  collectMovableInstructions(in: loop, &analyzedInstructions, &movableInstructions, context)
    
  return movableInstructions
}

/// Analyze instructions inside the `loop`. Compute side effects and populate `analyzedInstructions`.
///
/// - note: Ideally, `movableInstructions` should be fully computed in `collectMovableInstructions`.
private func analyzeInstructions(
  in loop: Loop,
  _ analyzedInstructions: inout AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  for bb in loop.loopBlocks {
    analyzedInstructions.markBeginOfBlock()
    
    for inst in bb.instructions {
      if inst.isImmediatelyHoistable {
        movableInstructions.immediatelyHoistable.append(inst)
        continue
      }
      
      switch inst {
      case is FixLifetimeInst:
        break // We can ignore the side effects of FixLifetimes
      case let loadInst as LoadInst:
        analyzedInstructions.loads.append(loadInst)
      case let uncheckedOwnershipConversionInst as UncheckedOwnershipConversionInst:
        analyzedInstructions.analyzeSideEffects(ofInst: uncheckedOwnershipConversionInst)
      case let storeInst as StoreInst:
        analyzedInstructions.stores.append(storeInst)
        analyzedInstructions.analyzeSideEffects(ofInst: storeInst)
      case let beginAccessInst as BeginAccessInst:
        analyzedInstructions.scopedInsts.append(beginAccessInst)
        analyzedInstructions.analyzeSideEffects(ofInst: beginAccessInst)
      case let beginBorrowInst as BeginBorrowInstruction:
        analyzedInstructions.analyzeSideEffects(ofInst: beginBorrowInst)
      case let condFailInst as CondFailInst:
        analyzedInstructions.analyzeSideEffects(ofInst: condFailInst)
      case let fullApply as FullApplySite:
        if fullApply.isSafeReadOnlyApply(context.calleeAnalysis) {
          analyzedInstructions.readOnlyApplies.append(fullApply)
        } else if let callee = fullApply.referencedFunction,
                  callee.isGlobalInitFunction, // Calls to global inits are different because we don't care about side effects which are "after" the call in the loop.
                  !fullApply.globalInitMayConflictWith(
                    blockSideEffectSegment: analyzedInstructions.sideEffectsOfCurrentBlock,
                    context.aliasAnalysis
                  ) {
          // Check against side-effects within the same block.
          // Side-effects in other blocks are checked later (after we
          // scanned all blocks of the loop) in `collectHoistableGlobalInitCalls`.
          analyzedInstructions.globalInitCalls.append(fullApply)
        }
        
        analyzedInstructions.fullApplies.append(fullApply)
        
        analyzedInstructions.analyzeSideEffects(ofInst: inst)
      case let builtinInst as BuiltinInst:
        switch builtinInst.id {
        case .Once, .OnceWithContext:
          if !builtinInst.globalInitMayConflictWith(
            blockSideEffectSegment: analyzedInstructions.sideEffectsOfCurrentBlock,
            context.aliasAnalysis
          ) {
            analyzedInstructions.globalInitCalls.append(builtinInst)
          }
        default: break
        }
        
        analyzedInstructions.analyzeSideEffects(ofInst: inst)
      default:
        analyzedInstructions.analyzeSideEffects(ofInst: inst)
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

/// Computes movable instructions using computed analyzed instructions.
private func collectMovableInstructions(
  in loop: Loop,
  _ analyzedInstructions: inout AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  var loadInstCounter = 0
  var readOnlyApplyCounter = 0
  for bb in loop.loopBlocks {
    for inst in bb.instructions where !inst.isImmediatelyHoistable {
      // Check against side effects which are "before" (i.e. post-dominated by) the global initializer call.
      //
      // The effects in the same block have already been checked before
      // adding this global init call to `analyzedInstructions.globalInitCalls` in `analyzeInstructions`.
      // TODO: Use stack for this check.
      if analyzedInstructions.globalInitCalls.contains(inst),
         inst.parentBlock.postDominates(loop.preheader!, context.postDominatorTree),
         !inst.globalInitMayConflictWith(
           loopSideEffects: analyzedInstructions.loopSideEffects,
           context.aliasAnalysis,
           context.postDominatorTree
         ) {
        movableInstructions.hoistUp.append(inst)
        continue
      }
      
      // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
      // TODO: Use stack for this check.
      if let fullApplySite = inst as? FullApplySite,
         analyzedInstructions.readOnlyApplies.contains(where: { $0 == fullApplySite }),
         readOnlyApplyCounter * analyzedInstructions.loopSideEffects.count < 8000,
         fullApplySite.isSafeReadOnlyApply(
           for: analyzedInstructions.loopSideEffects,
           context.aliasAnalysis,
           context.calleeAnalysis
         ) {
        if let beginApplyInst = fullApplySite as? BeginApplyInst {
          movableInstructions.scopedInsts.append(beginApplyInst)
        } else {
          movableInstructions.hoistUp.append(fullApplySite)
        }
        
        readOnlyApplyCounter += 1
        continue
      }
      
      switch inst {
      case let fixLifetimeInst as FixLifetimeInst:
        guard fixLifetimeInst.parentBlock.dominates(loop.preheader!, context.dominatorTree) else {
          continue
        }
        
        if !analyzedInstructions.sideEffectsMayRelease || !analyzedInstructions.loopSideEffectsMayWriteTo(address: fixLifetimeInst.operand.value, context.aliasAnalysis) {
          movableInstructions.sinkDown.append(fixLifetimeInst)
        }
      case let loadInst as LoadInst:
        // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
        if loadInstCounter * analyzedInstructions.loopSideEffects.count < 8000,
           !analyzedInstructions.loopSideEffectsMayWriteTo(address: loadInst.operand.value, context.aliasAnalysis) {
          movableInstructions.hoistUp.append(loadInst)
        }
        
        loadInstCounter += 1
        
        movableInstructions.loadsAndStores.append(loadInst)
      case is UncheckedOwnershipConversionInst:
        break // TODO: Add support
      case let storeInst as StoreInst:
        switch storeInst.storeOwnership {
        case .assign:
          continue  // TODO: Add support
        case .unqualified, .trivial, .initialize:
          break
        }
        movableInstructions.loadsAndStores.append(storeInst)
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
private func optimizeLoop(
  loop: Loop,
  movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) -> Bool {
  var changed = false
  
  // By hoisting `tuple_element_addr` and `struct_element_addr` here, `hoistAndSinkLoadsAndStores` could converge faster.
  changed = movableInstructions.immediatelyHoistInstructions(outOf: loop, context)    || changed
  changed = movableInstructions.hoistAndSinkLoadsAndStores(outOf: loop, context)      || changed
  changed = movableInstructions.hoistInstructions(outOf: loop, context)               || changed
  changed = movableInstructions.sinkInstructions(outOf: loop, context)                || changed
  changed = movableInstructions.hoistWithSinkScopedInstructions(outOf: loop, context) || changed

  return changed
}

extension BasicBlock {
  func containsStoresTo(accessPath: AccessPath) -> Bool {
    return instructions.contains { inst in
      return inst.operands.contains { operand in
        if let storeInst = operand.instruction as? StoreInst,
           storeInst.destination.accessPath == accessPath {
          return true
        } else {
          return false
        }
      }
    }
  }
}

private extension AnalyzedInstructions {
  /// Adds side effects of `inst` to the analyzed instructions.
  mutating func analyzeSideEffects(ofInst inst: Instruction) {
    if inst.mayHaveSideEffects {
      loopSideEffects.append(inst)
    } else if inst.mayReadFromMemory {
      hasOtherMemReadingInsts = true
    }
  }
  
  /// Returns true if all instructions in `sideEffects` which may alias with
  /// this path are either loads or stores from this path.
  ///
  /// `storeAddr` is only needed for AliasAnalysis until we have an interface
  /// that supports `AccessPath`.
  func isOnlyLoadedAndStored(
    accessPath: AccessPath,
    storeAddr: Value,
    _ aliasAnalysis: AliasAnalysis
  ) -> Bool {
    if (loopSideEffects.contains { sideEffect in
      switch sideEffect {
      case let storeInst as StoreInst:
        if storeInst.storesTo(accessPath) {
          return false
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
        
    if (loads.contains { loadInst in
      loadInst.mayRead(fromAddress: storeAddr, aliasAnalysis) && !loadInst.overlaps(accessPath: accessPath)
    }) {
      return false
    }
          
    if (stores.contains { storeInst in
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
         block.successors.filter({ $0.terminator is UnreachableInst }).count != block.successors.count {
        return false
      }
      
      worklist.pushIfNotVisited(contentsOf: block.successors)
    }
    
    return true
  }
  
  /// Returns true if `loopSideEffects` contains any memory writes which
  /// may alias with the memory `address`.
  func loopSideEffectsMayWriteTo(address: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    return loopSideEffects
      .contains { sideEffect in
        sideEffect.mayWrite(toAddress: address, aliasAnalysis)
      }
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

      guard !loadInst.isDeleted, loadInst.operand.value.accessPath.contains(accessPath) else {
        newLoads.push(loadInst)
        continue
      }

      guard let projectionPath =  loadInst.operand.value.accessPath.getProjection(to: accessPath),
            let splitLoads = loadInst.trySplit(alongPath: projectionPath, context) else {
        newLoads.push(loadInst)
        return false
      }
      
      splitCounter += splitLoads.count
      newLoads.append(contentsOf: splitLoads)
    }

    return true
  }
}

private extension MovableInstructions {
  /// Hoist instructions speculatively.
  ///
  /// Contrary to `hoistInstructions`, it doesn't only go through instructions in blocks that dominate all exits.
  mutating func immediatelyHoistInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false
    
    for inst in immediatelyHoistable {
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
    lazy var blocksDominatingExits = loop.getBlocksThatDominateAllExitingAndLatchBlocks(excludeColdExits: false, context)
    let blocksDominatingExitsExcludingCold = loop.getBlocksThatDominateAllExitingAndLatchBlocks(excludeColdExits: true, context)
    var changed = false

    for inst in hoistUp where blocksDominatingExitsExcludingCold.contains(inst.parentBlock) {
      if let valueInst = inst as? Value,
         valueInst.type.hasLocalArchetype,
         !blocksDominatingExits.contains(inst.parentBlock) {
        continue // Don't hoist speculatively value with local archetype.
      }
      
      changed = inst.hoist(outOf: loop, context) || changed
    }

    return changed
  }

  /// Sink instructions.
  mutating func sinkInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    let dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(excludeColdExits: true, context)
    var changed = false

    for inst in sinkDown where dominatingBlocks.contains(inst.parentBlock) {
      changed = inst.sink(outOf: loop, context) || changed
    }

    return changed
  }

  /// Hoist and sink scoped instructions.
  mutating func hoistWithSinkScopedInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    guard !loop.hasNoExitBlocks else {
      return false
    }
    
    var changed = false

    for scopedInst in scopedInsts {
      if let storeBorrowInst = scopedInst as? StoreBorrowInst {
        _ = storeBorrowInst.allocStack.hoist(outOf: loop, context)
        
        var sankFirst = false
        for deallocStack in storeBorrowInst.allocStack.deallocations {
          if sankFirst {
            context.erase(instruction: deallocStack)
          } else {
            sankFirst = deallocStack.sink(outOf: loop, context)
          }
        }
        
        context.notifyInvalidatedStackNesting()
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

  private mutating func hoistAndSinkLoadAndStore(
    outOf loop: Loop,
    accessPath: AccessPath,
    context: FunctionPassContext
  ) -> Bool {
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
    
    var ssaUpdater = SSAUpdater(
      function: firstStore.parentFunction,
      type: firstStore.destination.type.objectType,
      ownership: firstStore.source.ownership,
      context
    )
    
    // Set all stored values as available values in the ssaUpdater.
    for case let storeInst as StoreInst in loadsAndStores where storeInst.storesTo(accessPath) {
      ssaUpdater.addAvailableValue(storeInst.source, in: storeInst.parentBlock)
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
    
    let ownership: LoadInst.LoadOwnership = firstStore.parentFunction.hasOwnership ? (firstStore.storeOwnership == .initialize ? .take : .trivial) : .unqualified
    
    let initialLoad = builder.createLoad(fromAddress: initialAddr, ownership: ownership)
    ssaUpdater.addAvailableValue(initialLoad, in: loop.preheader!)
    
    var changed = false
    var currentBlock: BasicBlock?
    var currentVal: Value?
    
    // Remove all stores and replace the loads with the current value.
    //
    // This loop depends on loadsAndStores being in order the instructions appear in blocks.
    for inst in loadsAndStores {
      let block = inst.parentBlock
      
      if block != currentBlock {
        currentBlock = block
        currentVal = nil
      }
      
      if let storeInst = inst as? StoreInst, storeInst.storesTo(accessPath) {
        currentVal = storeInst.source
        context.erase(instruction: storeInst)
        changed = true
        continue
      }
      
      guard let loadInst = inst as? LoadInst,
            loadInst.loadsFrom(accessPath) else {
        continue
      }
      
      // If we didn't see a store in this block yet, get the current value from the ssaUpdater.
      let rootVal = currentVal ?? ssaUpdater.getValue(inMiddleOf: block)
      
      if loadInst.operand.value.accessPath == accessPath {
        loadInst.replace(with: rootVal, context)
        changed = true
        continue
      }
      
      guard let projectionPath = accessPath.getProjection(to: loadInst.operand.value.accessPath) else {
        continue
      }
    
      let builder = Builder(before: loadInst, context)
      let projection = rootVal.createProjection(path: projectionPath, builder: builder)
      loadInst.replace(with: projection, context)
      
      changed = true
    }
    
    loadsAndStores.removeAll(where: { $0.isDeleted })
    
    // Store back the value at all loop exits.
    for exitBlock in loop.exitBlocks {
      assert(exitBlock.hasSinglePredecessor, "Exiting edge should not be critical.")
      
      let builder = Builder(before: exitBlock.instructions.first!, context)
      
      builder.createStore(
        source: ssaUpdater.getValue(inMiddleOf: exitBlock),
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
}

private extension Instruction {
  /// Returns `true` if this instruction is a simple instruction that
  /// doesn't have any side effects and can be (speculatively) hoisted.
  var isImmediatelyHoistable: Bool {
    switch self {
    case is RefElementAddrInst, is IntegerLiteralInst, is StringLiteralInst,
         is BaseAddrForOffsetInst, is FloatLiteralInst, is FunctionRefInst,
         is GlobalAddrInst, is StructElementAddrInst, is TupleElementAddrInst,
         is VectorBaseAddrInst, is RefTailAddrInst:
      return true
    default:
      return false
    }
  }
  
  /// Returns `true` if this instruction follows the default hoisting heuristic which means it
  /// is not a terminator, allocation or deallocation and either a hoistable array semantics call or doesn't have memory effects.
  func canBeHoisted(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    switch self {
    case is TermInst, is Allocation, is Deallocation:
      return false
    case is ApplyInst:
      switch arraySemanticsCallKind {
      case .getCount, .getCapacity:
        if canHoistArraySemanticsCall(to: loop.preheader!.terminator, context) {
          return true
        }
      case .arrayPropsIsNativeTypeChecked:
        return false
      default:
        break
      }
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
      } else {
        move(before: terminator, context)
      }
    }
    
    if let singleValueInst = self as? SingleValueInstruction,
       !(self is ScopedInstruction || self is AllocStackInst),
       let identicalInst = (loop.preheader!.instructions.first { otherInst in
         return singleValueInst != otherInst && singleValueInst.isIdenticalTo(otherInst)
    }) {
      guard let identicalSingleValueInst = identicalInst as? SingleValueInstruction else {
        return true
      }
      
      singleValueInst.replace(with: identicalSingleValueInst, context)
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
    
    for exitBlock in loop.exitBlocks where !context.deadEndBlocks.isDeadEnd(exitBlock) {
      assert(exitBlock.hasSinglePredecessor, "Exiting edge should not be critical.")
      
      let exitBlockBuilder = Builder(before: exitBlock.instructions.first!, context)
      exitBlockBuilder.createEndBorrow(of: preheaderLoadBorrow)
    }
  }
  
  func sink(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false

    for exitBlock in loop.exitBlocks where !context.deadEndBlocks.isDeadEnd(exitBlock) {
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
      return mayWrite(toAddress: loadInst.operand.value, aliasAnalysis)
    case is CondFailInst:
      return false
    default:
      return true
    }
  }
  
  /// Returns `true` if any of the instructions in `sideEffects` cannot be
  /// reordered with a call to this global initializer (which is in the same basic
  /// block).
  func globalInitMayConflictWith(
    blockSideEffectSegment: StackWithCount<Instruction>.Segment,
    _ aliasAnalysis: AliasAnalysis
  ) -> Bool {
    return blockSideEffectSegment
      .contains { sideEffect in
        globalInitMayConflictWith(
          sideEffect: sideEffect,
          aliasAnalysis
        )
      }
  }

  /// Returns `true` if any of the instructions in `loopSideEffects` which are
  /// post-dominated by a call to this global initializer cannot be reordered with
  /// the call.
  func globalInitMayConflictWith(
    loopSideEffects: StackWithCount<Instruction>,
    _ aliasAnalysis: AliasAnalysis,
    _ postDomTree: PostDominatorTree
  ) -> Bool {
    return loopSideEffects
      .contains { sideEffect in
        // Only check instructions in blocks which are "before" (i.e. post-dominated
        // by) the block which contains the init-call.
        // Instructions which are before the call in the same block have already
        // been checked.
        parentBlock.strictlyPostDominates(sideEffect.parentBlock, postDomTree) &&
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
  /// Returns `true` if this load instruction loads from `accessPath` or a
  /// projected address from `accessPath`.
  func loadsFrom(_ accessPath: AccessPath) -> Bool {
    return accessPath.getProjection(to: self.address.accessPath)?.isMaterializable ?? false
  }
  
  func overlaps(accessPath: AccessPath) -> Bool {
    // Don't use `AccessPath.mayOverlap`. We only want definite overlap.
    return accessPath.isEqualOrContains(self.operand.value.accessPath) || self.operand.value.accessPath.isEqualOrContains(accessPath)
  }
}

private extension FullApplySite {
  /// Returns `true` if this apply inst could be safely hoisted.
  func isSafeReadOnlyApply(_ calleeAnalysis: CalleeAnalysis) -> Bool {
    guard functionConvention.results.allSatisfy({ $0.convention == .unowned }) else {
      return false
    }

    if let callee = referencedFunction,
       callee.hasSemanticsAttribute("array.props.isNativeTypeChecked") {
      return false
    }

    return !calleeAnalysis.getSideEffects(ofApply: self).memory.write
  }
  
  /// Returns `true` if the `sideEffects` contain any memory writes which
  /// may alias with any memory which is read by this `ApplyInst`.
  /// - Note: This function should only be called on a read-only apply!
  func isSafeReadOnlyApply(
    for sideEffects: StackWithCount<Instruction>,
    _ aliasAnalysis: AliasAnalysis,
    _ calleeAnalysis: CalleeAnalysis
  ) -> Bool {
    if calleeAnalysis.getSideEffects(ofApply: self).memory == .noEffects {
      return true
    }

    // Check if the memory addressed by the argument may alias any writes.
    for sideEffect in sideEffects {
      switch sideEffect {
      case let storeInst as StoreInst:
        if storeInst.storeOwnership == .assign ||
           mayRead(fromAddress: storeInst.destination, aliasAnalysis) {
          return false
        }
      case let copyAddrInst as CopyAddrInst:
        if !copyAddrInst.isInitializationOfDestination ||
           mayRead(fromAddress: copyAddrInst.destination, aliasAnalysis) {
          return false
        }
      case let fullApplySite as FullApplySite:
        if calleeAnalysis.getSideEffects(ofApply: fullApplySite).memory.write {
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
    
    // Instruction specific preconditions
    switch self {
    case is BeginAccessInst, is LoadBorrowInst:
      guard (analyzedInstructions.scopedInsts
        .allSatisfy { otherScopedInst in
          guard self != otherScopedInst else { return true }

          return operands.first!.value.accessPath.isDistinct(from: otherScopedInst.operand.value.accessPath)
        }) else {
        return false
      }
    default:
      break
    }
    
    var scope = InstructionRange(begin: self, ends: endInstructions, context)
    defer { scope.deinitialize() }
    
    // Instruction specific range related conditions
    switch self {
    case is BeginApplyInst:
      return true // Has already been checked with other full applies.
    case let loadBorrowInst as LoadBorrowInst:
      for case let destroyAddrInst as DestroyAddrInst in analyzedInstructions.loopSideEffects {
        if context.aliasAnalysis.mayAlias(loadBorrowInst.address, destroyAddrInst.destroyedAddress) {
          if !scope.contains(destroyAddrInst) {
            return false
          }
        }
      }
      
      for storeInst in analyzedInstructions.stores {
        if storeInst.mayWrite(toAddress: loadBorrowInst.address, context.aliasAnalysis) {
          if !scope.contains(storeInst) {
            return false
          }
        }
      }
      
      fallthrough
    case is BeginAccessInst:
      for fullApplyInst in analyzedInstructions.fullApplies {
        guard mayWriteToMemory && fullApplyInst.mayReadOrWrite(address: operands.first!.value, context.aliasAnalysis) ||
              !mayWriteToMemory && fullApplyInst.mayWrite(toAddress: operands.first!.value, context.aliasAnalysis) else {
          continue
        }

        // After hoisting the begin/end_access the apply will be within the scope, so it must not have a conflicting access.
        if !scope.contains(fullApplyInst) {
          return false
        }
      }
      
      switch operands.first!.value.accessPath.base {
      case .class, .global:
        for sideEffect in analyzedInstructions.loopSideEffects where sideEffect.mayRelease {
          // Since a class might have a deinitializer, hoisting begin/end_access pair could violate
          // exclusive access if the deinitializer accesses address used by begin_access.
          if !scope.contains(sideEffect) {
            return false
          }
        }

        return true
      default:
        return true
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
      .storeBorrow(let inst as Instruction),
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
