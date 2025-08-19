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
import OptimizerBridging

let loopInvariantCodeMotionPass = FunctionPass(name: "loop-invariant-code-motion") { function, context in
  for loop in context.loopTree.loops {
    optimizeTopLevelLoop(topLevelLoop: loop, context)
  }
}

private func optimizeTopLevelLoop(topLevelLoop: Loop, _ context: FunctionPassContext) {
  var innerLoops = Stack<Loop>(context)
  defer { innerLoops.deinitialize() }
  
  getWorkList(forLoop: topLevelLoop, result: &innerLoops)

  while let thisLoop = innerLoops.pop() {
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

private func getWorkList(forLoop loop: Loop, result: inout Stack<Loop>) {
  result.push(loop)
  
  for innerLoop in loop.innerLoops {
    getWorkList(forLoop: innerLoop, result: &result)
  }
}

private struct MovableInstructions {
  var loadAndStoreAddrs: [AccessPath] = []
  
  var speculativelyHoistable: [Instruction] = []
  var loadsAndStores: [Instruction] = []
  var hoistUp: [Instruction] = []
  var sinkDown: [Instruction] = []
  var scopedInsts: [Instruction] = []
}

private struct AnalyzedInstructions {
  var loopSideEffects: StackWithCount<Instruction>
  private var blockSideEffectBottomMarker: StackWithCount<Instruction>.Marker
  var sideEffectsOfCurrentBlock: StackWithCount<Instruction>.Segment {
    return StackWithCount<Instruction>.Segment(
      in: loopSideEffects,
      low: blockSideEffectBottomMarker,
      high: loopSideEffects.top
    )
  }
  
  var globalInitCalls: Stack<Instruction>
  var readOnlyApplies: Stack<ApplyInst>
  var loads: Stack<LoadInst>
  var stores: Stack<StoreInst>
  var beginAccesses: Stack<BeginAccessInst>
  var fullApplies: Stack<FullApplySite>
  
  var hasOtherMemReadingInsts = false
  
  lazy var sideEffectsMayRelease = loopSideEffects.contains(where: { $0.mayRelease })
  
  init (_ context: FunctionPassContext) {
    self.loopSideEffects = StackWithCount<Instruction>(context)
    self.blockSideEffectBottomMarker = loopSideEffects.top
    
    self.globalInitCalls = Stack<Instruction>(context)
    self.readOnlyApplies = Stack<ApplyInst>(context)
    self.loads = Stack<LoadInst>(context)
    self.stores = Stack<StoreInst>(context)
    self.beginAccesses = Stack<BeginAccessInst>(context)
    self.fullApplies = Stack<FullApplySite>(context)
  }
  
  mutating func deinitialize() {
    readOnlyApplies.deinitialize()
    globalInitCalls.deinitialize()
    loopSideEffects.deinitialize()
    loads.deinitialize()
    stores.deinitialize()
    beginAccesses.deinitialize()
    fullApplies.deinitialize()
  }
  
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
  loop.splitCriticalEdges(context)

  var movableInstructions = MovableInstructions()
  var analyzedInstructions = AnalyzedInstructions(context)
  defer { analyzedInstructions.deinitialize() }

  analyzeInstructions(in: loop, &analyzedInstructions, &movableInstructions, context)

  collectHoistableReadOnlyApplies(analyzedInstructions, &movableInstructions, context)

  collectHoistableGlobalInitCalls(in: loop, analyzedInstructions, &movableInstructions, context)

  collectProjectableAccessPathsAndSplitLoads(in: loop, &analyzedInstructions, &movableInstructions, context)
  
  collectMovableInstructions(in: loop, &analyzedInstructions, &movableInstructions, context)
    
  return movableInstructions
}

private func analyzeInstructions(
  in loop: Loop,
  _ analyzedInstructions: inout AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  for bb in loop.loopBlocks {
    analyzedInstructions.markBeginOfBlock()
    
    for inst in bb.instructions {
      // TODO: Remove once support for values with ownership implemented.
      if inst.hasOwnershipOperandsOrResults {
        analyzedInstructions.analyzeSideEffects(ofInst: inst)
        
        if let fullApply = inst as? FullApplySite {
          analyzedInstructions.fullApplies.append(fullApply)
        }
        
        continue
      }
      
      switch inst {
      case is FixLifetimeInst:
        break
      case let loadInst as LoadInst:
        analyzedInstructions.loads.append(loadInst)
      case let storeInst as StoreInst:
        switch storeInst.storeOwnership {
        case .assign, .initialize:
          continue  // TODO: Add support
        case .unqualified, .trivial:
          break
        }
        analyzedInstructions.stores.append(storeInst)
        analyzedInstructions.analyzeSideEffects(ofInst: storeInst)
      case let beginAccessInst as BeginAccessInst:
        analyzedInstructions.beginAccesses.append(beginAccessInst)
        analyzedInstructions.analyzeSideEffects(ofInst: beginAccessInst)
      case let refElementAddrInst as RefElementAddrInst:
        movableInstructions.speculativelyHoistable.append(refElementAddrInst)
      case let condFailInst as CondFailInst:
        analyzedInstructions.analyzeSideEffects(ofInst: condFailInst)
      case let applyInst as ApplyInst:
        if applyInst.isSafeReadOnlyApply(context.calleeAnalysis) {
          analyzedInstructions.readOnlyApplies.append(applyInst)
        } else if let callee = applyInst.referencedFunction,
                  callee.isGlobalInitFunction,
                  !applyInst.globalInitMayConflictWith(
                    blockSideEffectSegment: analyzedInstructions.sideEffectsOfCurrentBlock,
                    context.aliasAnalysis
                  ) {
          analyzedInstructions.globalInitCalls.append(applyInst)
        }
        
        fallthrough
      default:
        switch inst {
        case let fullApply as FullApplySite:
          analyzedInstructions.fullApplies.append(fullApply)
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
        default: break
        }
        
        analyzedInstructions.analyzeSideEffects(ofInst: inst)
        
        if inst.canBeHoisted(outOf: loop, context) {
          movableInstructions.hoistUp.append(inst)
        }
      }
    }
  }
}

private func collectHoistableReadOnlyApplies(
  _ analyzedInstructions: AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  var counter = 0
  for readOnlyApply in analyzedInstructions.readOnlyApplies {
    // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
    if counter * analyzedInstructions.loopSideEffects.count < 8000, readOnlyApply.isSafeReadOnlyApply(
      for: analyzedInstructions.loopSideEffects,
      context.aliasAnalysis,
      context.calleeAnalysis
    ) {
      movableInstructions.hoistUp.append(readOnlyApply)
    }
    counter += 1
  }
}

private func collectHoistableGlobalInitCalls(
  in loop: Loop,
  _ analyzedInstructions: AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  for globalInitCall in analyzedInstructions.globalInitCalls {
    if !globalInitCall.globalInitMayConflictWith(
      loopSideEffects: analyzedInstructions.loopSideEffects,
      notPostDominatingPreheader: loop.preheader!,
      context.aliasAnalysis,
      context.postDominatorTree
    ) {
      movableInstructions.hoistUp.append(globalInitCall)
    }
  }
}

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
         accessPath.isOnlyLoadedAndStored(
          storeAddr: storeInst.destination,
          analyzedInstructions: analyzedInstructions,
          context.aliasAnalysis
         ),
         !movableInstructions.loadAndStoreAddrs.contains(accessPath),
         loop.storesCommonlyDominateExits(analyzedInstructions: analyzedInstructions, accessPath: accessPath, context),
         analyzedInstructions.splitLoads(
          storeAddr: storeInst.destination,
          accessPath: accessPath,
          context
         ) {
        movableInstructions.loadAndStoreAddrs.append(accessPath)
      }
    }
  }
}

private func collectMovableInstructions(
  in loop: Loop,
  _ analyzedInstructions: inout AnalyzedInstructions,
  _ movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) {
  var loadInstCounter = 0
  for bb in loop.loopBlocks {
    for inst in bb.instructions {
      // TODO: Remove once support for values with ownership implemented.
      if inst.hasOwnershipOperandsOrResults {
        continue
      }
      
      switch inst {
      case let fixLifetimeInst as FixLifetimeInst:
        guard fixLifetimeInst.parentBlock.dominates(loop.preheader!, context.dominatorTree) else {
          continue
        }
        
        if !analyzedInstructions.sideEffectsMayRelease || !fixLifetimeInst.mayWriteTo(
          sideEffects: analyzedInstructions.loopSideEffects,
          context.aliasAnalysis
        ) {
          movableInstructions.sinkDown.append(fixLifetimeInst)
        }
      case let loadInst as LoadInst:
        // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
        if loadInstCounter * analyzedInstructions.loopSideEffects.count < 8000,
           !loadInst.mayWriteTo(sideEffects: analyzedInstructions.loopSideEffects, context.aliasAnalysis) {
          movableInstructions.hoistUp.append(loadInst)
        }
        
        loadInstCounter += 1
        
        movableInstructions.loadsAndStores.append(loadInst)
      case let storeInst as StoreInst:
        switch storeInst.storeOwnership {
        case .assign, .initialize:
          continue  // TODO: Add support
        case .unqualified, .trivial:
          break
        }
        movableInstructions.loadsAndStores.append(storeInst)
      case let condFailInst as CondFailInst:
        movableInstructions.hoistUp.append(condFailInst)
      case let beginAccessInst as BeginAccessInst:
        if beginAccessInst.canBeHoisted(outOf: loop, analyzedInstructions: analyzedInstructions, context) {
          movableInstructions.scopedInsts.append(beginAccessInst)
        }
      default:
        break
      }
    }
  }
}

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
  changed = movableInstructions.sinkInstructions(outOf: loop, context)                || changed
  changed = movableInstructions.hoistWithSinkScopedInstructions(outOf: loop, context) || changed

  return changed
}

private extension AnalyzedInstructions {
  mutating func analyzeSideEffects(ofInst inst: Instruction) {
    if inst.mayHaveSideEffects {
      loopSideEffects.append(inst)
    } else if inst.mayReadFromMemory {
      hasOtherMemReadingInsts = true
    }
  }
  
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
      guard splitCounter <= 6 else {
        newLoads.push(loadInst)
        return false
      }

      guard !loadInst.isDeleted, loadInst.operand.value.accessPath.contains(accessPath) else {
        newLoads.push(loadInst)
        continue
      }

      if let splitLoads = loadInst.trySplit(alongPath: accessPath.projectionPath, context) {
        splitCounter += splitLoads.count
        newLoads.append(contentsOf: splitLoads)
      }
    }

    return true
  }
}

private extension MovableInstructions {
  mutating func speculativelyHoistInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false
    for inst in speculativelyHoistable {
      changed = inst.hoist(outOf: loop, context) || changed
    }
    return changed
  }
  
  mutating func hoistAndSinkLoadsAndStores(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false
    for accessPath in loadAndStoreAddrs {
      changed = hoistAndSinkLoadAndStore(outOf: loop, accessPath: accessPath, context: context) || changed
    }
    
    return changed
  }
  
  mutating func hoistInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    let dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(domTree: context.dominatorTree)
    var changed = false

    // Only traverse instructions in blocks that dominate all exit and latch blocks.
    // We don't hoist instructions speculatively here.
    for bb in dominatingBlocks {
      for inst in bb.instructions where hoistUp.contains(inst) {
        changed = inst.hoist(outOf: loop, context) || changed
      }
    }

    return changed
  }

  mutating func sinkInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    let dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(domTree: context.dominatorTree)
    var changed = false

    for inst in sinkDown where dominatingBlocks.contains(inst.parentBlock) {
      changed = inst.sink(outOf: loop, context) || changed
    }

    return changed
  }

  mutating func hoistWithSinkScopedInstructions(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    guard !loop.hasNoExitBlocks else {
      return false
    }
    
    var changed = false

    for specialInst in scopedInsts {
      guard let beginAccessInst = specialInst as? BeginAccessInst else {
        continue
      }

      guard specialInst.hoist(outOf: loop, context) else {
        continue
      }

      for endAccess in beginAccessInst.endAccessInstructions {
        _ = endAccess.sink(outOf: loop, context)
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
    var changed = false
    let builder = Builder(before: loop.preheader!.terminator, context)
    var storeAddr: Value?
    
    for case let storeInst as StoreInst in loadsAndStores where storeInst.storesTo(accessPath) {
      if let srcLoadInst = storeInst.source as? LoadInst,
         srcLoadInst.loadsFrom(accessPath) {
        return changed
      }
      
      if storeAddr == nil {
        storeAddr = storeInst.destination
      } else if storeInst.destination.type != storeAddr!.type {
        return changed
      }
    }
    
    guard let storeAddr else {
      return changed
    }
    
    var ssaUpdater = SSAUpdater(
      function: storeAddr.parentFunction,
      type: storeAddr.type.objectType,
      ownership: storeAddr.ownership,
      context
    )
    
    for case let storeInst as StoreInst in loadsAndStores where storeInst.storesTo(accessPath) {
      ssaUpdater.addAvailableValue(storeInst.source, in: storeInst.parentBlock)
    }
    
    var cloner = Cloner(cloneBefore: loop.preheader!.terminator, context)
    defer { cloner.deinitialize() }
    
    guard let initialAddr = (cloner.cloneAddressProjections(addr: storeAddr) { srcAddr in
      srcAddr.parentBlock.dominates(loop.preheader!, context.dominatorTree)
    }) else {
      return changed
    }
    
    let ownership: LoadInst.LoadOwnership = loop.preheader!.terminator.parentFunction.hasOwnership ? .trivial : .unqualified
    
    let initialLoad = builder.createLoad(fromAddress: initialAddr, ownership: ownership)
    ssaUpdater.addAvailableValue(initialLoad, in: loop.preheader!)
    
    var currentBlock: BasicBlock?
    var currentVal: Value?
    
    // This loop depends on loadsAndStores being
    // in order the instructions appear in blocks!
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
    
    for exitingOrLatchBlock in loop.exitingAndLatchBlocks {
      for succesor in exitingOrLatchBlock.successors where !loop.contains(block: succesor) {
        guard let firstInst = succesor.instructions.first else {
          continue
        }
        
        let builder = Builder(before: firstInst, context)
        
        let ownership: StoreInst.StoreOwnership = firstInst.parentFunction.hasOwnership ? .trivial : .unqualified
        builder.createStore(
          source: ssaUpdater.getValue(inMiddleOf: succesor),
          destination: initialAddr,
          ownership: ownership
        )
        changed = true
      }
    }
    
    if initialLoad.uses.isEmpty {
      context.erase(instruction: initialLoad)
    }
    
    return changed
  }
}

private extension Loop {
  /// Returns `true` if all stores to `accessPath` commonly dominate the loop exits.
  func storesCommonlyDominateExits(
    analyzedInstructions: AnalyzedInstructions,
    accessPath: AccessPath,
    _ context: FunctionPassContext
  ) -> Bool {
    var storeBlocks = BasicBlockSet(context)
    var worklist = BasicBlockWorklist(context)
    defer {
      storeBlocks.deinitialize()
      worklist.deinitialize()
    }
    
    if preheader!.instructions.contains(where: { $0.operands.contains(where: { operand in
      operand.instruction is StoreInst && operand.value.accessPath == accessPath
    })}) {
      return true
    }
    
    storeBlocks.insert(contentsOf: analyzedInstructions.stores
      .filter({ $0.destination.accessPath == accessPath })
      .map(\.parentBlock))
    
    if storeBlocks.contains(header) {
      return true
    }
    
    worklist.pushIfNotVisited(header)
    while let block = worklist.pop() {
      if storeBlocks.contains(block) {
        continue
      }
      
      if exitingBlocks.contains(block) && !block.successors.contains(where: {$0.terminator is UnreachableInst}) {
        return false
      }
      
      worklist.pushIfNotVisited(contentsOf: block.successors)
    }
    
    return true
  }
}

private extension Instruction {
  var hasOwnershipOperandsOrResults: Bool {
    guard parentFunction.hasOwnership else { return false }

    return results.contains(where: { $0.ownership != .none })
      || operands.contains(where: { $0.value.ownership != .none })
  }
  
  /// Returns `true` if this instruction follows the default hoisting heuristic.
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

    if memoryEffects == .noEffects {
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
      move(before: terminator, context)
    }
    
    if let singleValueInst = self as? SingleValueInstruction,
       !(self is BeginAccessInst),
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
  
  func sink(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    var canMove = loop.hasSingleExitBlock
    let exitBlocks = loop.exitBlocks
    let exitingBlocks = loop.exitingBlocks
    var newExitBlocks = Stack<BasicBlock>(context)
    defer { newExitBlocks.deinitialize() }
    var changed = false

    for exitingBlock in exitingBlocks {
      for succesor in exitingBlock.successors where !newExitBlocks.contains(succesor) && exitBlocks.contains(succesor) {

        newExitBlocks.push(succesor)

        if succesor.instructions.contains(where: { isIdenticalTo($0) }) {
          canMove = false
        } else if canMove {
          move(before: succesor.instructions.first!, context)
        } else if let firstInstruction = succesor.instructions.first {
          copy(before: firstInstruction, context)
        } else {
          continue
        }

        changed = true
      }
    }

    if changed && !canMove {
      context.erase(instruction: self)
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
    notPostDominatingPreheader: BasicBlock,
    _ aliasAnalysis: AliasAnalysis,
    _ postDomTree: PostDominatorTree
  ) -> Bool {
    guard parentBlock.postDominates(notPostDominatingPreheader, postDomTree) else {
      return true
    }

    return loopSideEffects
      .contains { sideEffect in
        // Only check instructions in blocks which are "before" (i.e. post-dominated
        // by) the block which contains the init-call.
        // Instructions which are before the call in the same block have already
        // been checked.
        parentBlock.strictlyPostDominates(
          sideEffect.parentBlock,
          postDomTree
        ) && globalInitMayConflictWith(
          sideEffect: sideEffect,
          aliasAnalysis
        )
      }
  }
  
  func isInAccessScope(of beginAccess: BeginAccessInst, _ domTree: DominatorTree) -> Bool {
    return beginAccess.dominates(self, domTree) && beginAccess.endAccessInstructions
      .allSatisfy { endAccessInst in
        self.dominates(endAccessInst, domTree)
      }
  }
}

private extension UnaryInstruction {
  /// Returns true if `sideEffects` contains any memory writes which
  /// may alias with the memory addressed by this instruction.
  func mayWriteTo(
    sideEffects: StackWithCount<Instruction>,
    _ aliasAnalysis: AliasAnalysis
  ) -> Bool {
    return sideEffects
      .contains { sideEffect in
        sideEffect.mayWrite(toAddress: operand.value, aliasAnalysis)
      }
  }
}

private extension StoreInst {
  /// Returns a `true` if this store is a store to `accessPath`.
  func storesTo(_ accessPath: AccessPath) -> Bool {
    guard self.storeOwnership != .initialize else {
      return false
    }

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
    guard self.loadOwnership != .take else { // TODO: handle LoadOwnershipQualifier::Take
      return false
    }
    
    return accessPath.isEqualOrContains(self.operand.value.accessPath) || self.operand.value.accessPath.isEqualOrContains(accessPath)
  }
}

private extension ApplyInst {
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
  
  func isSafeReadOnlyApply(
    for sideEffects: StackWithCount<Instruction>,
    _ aliasAnalysis: AliasAnalysis,
    _ calleeAnalysis: CalleeAnalysis
  ) -> Bool {
    if calleeAnalysis.getSideEffects(ofApply: self).memory == .noEffects {
      return true
    }

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
      case is ApplyInst, is BeginApplyInst, is TryApplyInst:
        if calleeAnalysis.getSideEffects(ofApply: self).memory.write {
          return false
        }
      case is CondFailInst, is StrongRetainInst, is UnmanagedRetainValueInst,
        is RetainValueInst, is StrongRetainUnownedInst, is FixLifetimeInst,
        is KeyPathInst, is DeallocStackInst, is DeallocStackRefInst,
        is DeallocRefInst:
        break
      default:
        if sideEffect.mayWriteToMemory {
          return false
        }
      }
    }

    return true
  }
}

private extension BeginAccessInst {
  func canBeHoisted(
    outOf loop: Loop,
    analyzedInstructions: AnalyzedInstructions,
    _ context: FunctionPassContext
  ) -> Bool {
    guard endAccessInstructions.allSatisfy({ loop.contains(block: $0.parentBlock)}) else {
      return false
    }
    
    let areBeginAccessesSafe = analyzedInstructions.beginAccesses
      .allSatisfy { otherBeginAccessInst in
        guard self != otherBeginAccessInst else { return true }

        return self.accessPath.isDistinct(from: otherBeginAccessInst.accessPath)
      }

    guard areBeginAccessesSafe else { return false }
    
    var scope = InstructionRange(begin: self, ends: endAccessInstructions, context)
    defer { scope.deinitialize() }

    for fullApplyInst in analyzedInstructions.fullApplies {
      guard mayWriteToMemory
          ? fullApplyInst.mayReadOrWrite(
            address: address,
            context.aliasAnalysis
          ) : fullApplyInst.mayWrite(
            toAddress: address,
            context.aliasAnalysis
          ) else {
        continue
      }

      if !scope.contains(fullApplyInst) {
        return false
      }
    }

    switch accessPath.base {
    case .class, .global:
      for sideEffect in analyzedInstructions.loopSideEffects where sideEffect.mayRelease {
        if !scope.contains(sideEffect) {
          return false
        }
      }

      return true
    default:
      return true
    }
  }
}

private extension AccessPath {
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
  
  /// Returns true if all instructions in `sideEffects` which may alias with
  /// this path are either loads or stores from this path.
  ///
  /// `storeAddr` is only needed for AliasAnalysis until we have an interface
  /// that supports `AccessPath`.
  func isOnlyLoadedAndStored(
    storeAddr: Value,
    analyzedInstructions: AnalyzedInstructions,
    _ aliasAnalysis: AliasAnalysis
  ) -> Bool {
    if (analyzedInstructions.loopSideEffects.contains { sideEffect in
      let accessesPath: Bool
      switch sideEffect {
      case let storeInst as StoreInst:
        accessesPath = storeInst.storesTo(self)
      case let loadInst as LoadInst:
        accessesPath = loadInst.loadsFrom(self)
      default:
        accessesPath = false
      }
      
      return !accessesPath && sideEffect.mayReadOrWrite(address: storeAddr, aliasAnalysis)
    }) {
      return false
    }
        
    if (analyzedInstructions.loads.contains { loadInst in
      loadInst.mayRead(fromAddress: storeAddr, aliasAnalysis) && !loadInst.overlaps(accessPath: self)
    }) {
      return false
    }
          
    if (analyzedInstructions.stores.contains { storeInst in
      storeInst.mayWrite(toAddress: storeAddr, aliasAnalysis) && !storeInst.storesTo(self)
    }) {
      return false
    }
    
    return true
  }
}
