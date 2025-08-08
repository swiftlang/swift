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
    var thisLoopChanged = false

    repeat {
      if var movableInstructions = analyzeLoopAndSplitLoads(loop: thisLoop, context) {
        thisLoopChanged = optimizeLoop(loop: thisLoop, movableInstructions: &movableInstructions, context)
      }
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
  
  var simplyHoistableInsts: [Instruction] = []
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
  var readOnlyApplies: StackWithCount<ApplyInst>
  var loads: StackWithCount<LoadInst>
  var stores: Stack<StoreInst>
  var beginAccesses: Stack<BeginAccessInst>
  var fullApplies: Stack<FullApplySite>
  
  var hasOtherMemReadingInsts = false
  
  lazy var sideEffectsMayRelease = loopSideEffects.contains(where: { $0.mayRelease })
  
  init (_ context: FunctionPassContext) {
    self.loopSideEffects = StackWithCount<Instruction>(context)
    self.blockSideEffectBottomMarker = loopSideEffects.top
    
    self.globalInitCalls = Stack<Instruction>(context)
    self.readOnlyApplies = StackWithCount<ApplyInst>(context)
    self.loads = StackWithCount<LoadInst>(context)
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

private func analyzeLoopAndSplitLoads(loop: Loop, _ context: FunctionPassContext) -> MovableInstructions? {
  guard let preheader = loop.preheader else {
    return nil
  }
  
  loop.splitCriticalEdges(context)

  var movableInstructions = MovableInstructions()
  var analyzedInstructions = AnalyzedInstructions(context)
  defer { analyzedInstructions.deinitialize() }

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
        movableInstructions.simplyHoistableInsts.append(refElementAddrInst)
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

  // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
  if analyzedInstructions.readOnlyApplies.count * analyzedInstructions.loopSideEffects.count < 8000 {
    for readOnlyApply in analyzedInstructions.readOnlyApplies {
      if !readOnlyApply.isSafeReadOnlyApply(
        for: analyzedInstructions.loopSideEffects,
        context.aliasAnalysis,
        context.calleeAnalysis
      ) {
        movableInstructions.hoistUp.append(readOnlyApply)
      }
    }
  }

  for globalInitCall in analyzedInstructions.globalInitCalls {
    if !globalInitCall.globalInitMayConflictWith(
      loopSideEffects: analyzedInstructions.loopSideEffects,
      notPostDominatingPreheader: preheader,
      context.aliasAnalysis,
      context.postDominatorTree
    ) {
      movableInstructions.hoistUp.append(globalInitCall)
    }
  }

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
  
  for bb in loop.loopBlocks {
    for inst in bb.instructions {
      // TODO: Remove once support for values with ownership implemented.
      if inst.hasOwnershipOperandsOrResults {
        continue
      }
      
      switch inst {
      case let fixLifetimeInst as FixLifetimeInst:
        guard fixLifetimeInst.parentBlock.dominates(preheader, context.dominatorTree) else {
          continue
        }
        
        if analyzedInstructions.sideEffectsMayRelease || !fixLifetimeInst.mayWriteTo(
          sideEffects: analyzedInstructions.loopSideEffects,
          context.aliasAnalysis
        ) {
          movableInstructions.sinkDown.append(fixLifetimeInst)
        }
      case let loadInst as LoadInst:
        // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
        if analyzedInstructions.loads.count * analyzedInstructions.loopSideEffects.count < 8000,
           !loadInst.mayWriteTo(sideEffects: analyzedInstructions.loopSideEffects, context.aliasAnalysis) {
          movableInstructions.hoistUp.append(loadInst)
        }
        
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
    
  return movableInstructions
}

private func optimizeLoop(
  loop: Loop,
  movableInstructions: inout MovableInstructions,
  _ context: FunctionPassContext
) -> Bool {
  var changed = false
  
  // TODO: If we hoist tuple_element_addr and struct_element_addr instructions here, hoistAndSinkLoadsAndStores could converge after just one execution!
  changed = movableInstructions.simpleHoistInstructions(loop: loop, context)         || changed
  changed = movableInstructions.hoistAndSinkLoadsAndStores(outOf: loop, context)     || changed
  changed = movableInstructions.hoistInstructions(loop: loop, context)               || changed
  changed = movableInstructions.sinkInstructions(loop: loop, context)                || changed
  changed = movableInstructions.hoistWithSinkScopedInstructions(loop: loop, context) || changed

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
  mutating func simpleHoistInstructions(loop: Loop, _ context: FunctionPassContext) -> Bool {
    var changed = false
    for inst in simplyHoistableInsts {
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
  
  mutating func hoistInstructions(loop: Loop, _ context: FunctionPassContext) -> Bool {
    let dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(domTree: context.dominatorTree)
    var changed = false

    for bb in dominatingBlocks {
      for inst in bb.instructions where hoistUp.contains(inst) {
        changed = inst.hoist(outOf: loop, context) || changed
      }
    }

    return changed
  }

  mutating func sinkInstructions(loop: Loop, _ context: FunctionPassContext) -> Bool {
    let dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(domTree: context.dominatorTree)
    var changed = false

    for inst in sinkDown where dominatingBlocks.contains(inst.parentBlock) {
      changed = inst.sink(outOf: loop, context) || changed
    }

    return changed
  }

  mutating func hoistWithSinkScopedInstructions(loop: Loop, _ context: FunctionPassContext) -> Bool {
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
    let exitingAndLatchBlocks = loop.exitingAndLatchBlocks
    
    guard let preheader = loop.preheader else {
      return false
    }
    
    var changed = false
    let builder = Builder(before: preheader.terminator, context)
    var ssaUpdater: SSAUpdater<FunctionPassContext>?
    var storeAddr: Value?
    
    for case let storeInst as StoreInst in loadsAndStores where storeInst.storesTo(accessPath) {
      if let srcLoadInst = storeInst.source as? LoadInst,
         srcLoadInst.loadsFrom(accessPath) {
        return changed
      }
      
      if storeAddr == nil {
        storeAddr = storeInst.destination
        ssaUpdater = SSAUpdater(
          function: storeAddr!.parentFunction,
          type: storeAddr!.type.objectType,
          ownership: storeAddr!.ownership,
          context
        )
      } else if storeInst.destination.type != storeAddr!.type {
        return changed
      }
      
      ssaUpdater?.addAvailableValue(storeInst.source, in: storeInst.parentBlock)
    }
    
    guard let storeAddr, var ssaUpdater else {
      return changed
    }
    
    var cloner = Cloner(cloneBefore: preheader.terminator, context)
    defer { cloner.deinitialize() }
    
    guard let initialAddr = (cloner.cloneUseDefChain(addr: storeAddr) { srcAddr in
      srcAddr.parentBlock.dominates(preheader, context.dominatorTree)
    }) else {
      return changed
    }
    
    let ownership: LoadInst.LoadOwnership = preheader.terminator.parentFunction.hasOwnership ? .trivial : .unqualified
    
    let initialLoad = builder.createLoad(fromAddress: initialAddr, ownership: ownership)
    ssaUpdater.addAvailableValue(initialLoad, in: preheader)
    
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
    
      guard loadInst.replaceLoadWithProjection(
        rootVal: rootVal,
        rootAccessPath: accessPath,
        beforeInst: loadInst,
        context
      ) else {
        continue
      }
      
      changed = true
    }
    
    loadsAndStores.removeAll(where: { $0.isDeleted })
    
    for exitingOrLatchBlock in exitingAndLatchBlocks {
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
      storeBlocks.insert(preheader!)
    }
    
    storeBlocks.insert(contentsOf: analyzedInstructions.stores
      .filter({ $0.destination.accessPath == accessPath })
      .map(\.parentBlock))
    
    if storeBlocks.contains(preheader!) || storeBlocks.contains(header) {
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
  
  func canBeHoisted(outOf loop: Loop, _ context: FunctionPassContext) -> Bool {
    guard let preheader = loop.preheader else {
      return false
    }

    switch self {
    case is TermInst, is Allocation, is Deallocation:
      return false
    case is ApplyInst:
      switch arraySemanticsCallKind {
      case .getCount, .getCapacity:
        if canHoistArraySemanticsCall(to: preheader.terminator, context) {
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
    guard let preheader = loop.preheader,
          operands.allSatisfy({ !loop.contains(block: $0.value.parentBlock) }) else {
      return false
    }

    let terminator = preheader.terminator
    if canHoistArraySemanticsCall(to: terminator, context) {
      hoistArraySemanticsCall(before: terminator, context)
    } else {
      move(before: terminator, context)
    }
    
    if let singleValueInst = self as? SingleValueInstruction,
       !(self is BeginAccessInst),
       let identicalInst = (preheader.instructions.first { otherInst in
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
    var canMove = loop.isSingleExit
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
  func storesTo(_ accessPath: AccessPath) -> Bool {
    guard self.storeOwnership != .initialize else {
      return false
    }

    return accessPath == self.destination.accessPath
  }
}

private extension LoadInst {
  func replaceLoadWithProjection(
    rootVal: Value,
    rootAccessPath: AccessPath,
    beforeInst: Instruction,
    _ context: FunctionPassContext
  ) -> Bool {
    guard operand.value.accessPath != rootAccessPath else {
      replace(with: rootVal, context)
      return true
    }
    
    guard let projectionPath = rootAccessPath.getProjection(to: operand.value.accessPath) else {
      return false
    }
    
    let builder = Builder(before: beforeInst, context)
    
    var currPath = projectionPath
    var currVal = rootVal
    
    while !currPath.isEmpty {
      let (kind, index, remainderPath) = currPath.pop()
      
      switch kind {
      case .structField:
        currVal = builder.createStructExtract(struct: currVal, fieldIndex: index)
      case .tupleField:
        currVal = builder.createTupleExtract(tuple: currVal, elementIndex: index)
      default:
        return false
      }
      
      currPath = remainderPath
    }
    
    replace(with: currVal, context)
    return true
  }
  
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

    return calleeAnalysis.getSideEffects(ofApply: self).isOnlyReading
  }
  
  func isSafeReadOnlyApply(
    for sideEffects: StackWithCount<Instruction>,
    _ aliasAnalysis: AliasAnalysis,
    _ calleeAnalysis: CalleeAnalysis
  ) -> Bool {
    if calleeAnalysis.getSideEffects(ofApply: self).memory == .noEffects {
      return false
    }

    for sideEffect in sideEffects {
      switch sideEffect {
      case let storeInst as StoreInst:
        if storeInst.storeOwnership == .assign ||
           mayRead(fromAddress: storeInst.destination, aliasAnalysis) {
          return true
        }
      case let copyAddrInst as CopyAddrInst:
        if !copyAddrInst.isInitializationOfDestination ||
           mayRead(fromAddress: copyAddrInst.destination, aliasAnalysis) {
          return true
        }
      case is ApplyInst, is BeginApplyInst, is TryApplyInst:
        if !calleeAnalysis.getSideEffects(ofApply: self).isOnlyReading {
          return true
        }
      case is CondFailInst, is StrongRetainInst, is UnmanagedRetainValueInst,
        is RetainValueInst, is StrongRetainUnownedInst, is FixLifetimeInst,
        is KeyPathInst, is DeallocStackInst, is DeallocStackRefInst,
        is DeallocRefInst:
        break
      default:
        if sideEffect.mayWriteToMemory {
          return true
        }
      }
    }

    return false
  }
}

private extension BeginAccessInst {
  private func handlesAllEndAccesses(loop: Loop) -> Bool {
    return !endAccessInstructions.isEmpty && !endAccessInstructions
        .contains { user in
          !loop.contains(block: user.parentBlock)
        }
  }
  
  func canBeHoisted(
    outOf loop: Loop,
    analyzedInstructions: AnalyzedInstructions,
    _ context: FunctionPassContext
  ) -> Bool {
    guard handlesAllEndAccesses(loop: loop) else {
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
  
  func isOnlyLoadedAndStored(
    storeAddr: Value,
    analyzedInstructions: AnalyzedInstructions,
    _ aliasAnalysis: AliasAnalysis
  ) -> Bool {
    return !analyzedInstructions.loopSideEffects
      .contains { sideEffect in
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
      } && !analyzedInstructions.loads
      .contains { loadInst in
        loadInst.mayRead(fromAddress: storeAddr, aliasAnalysis)
        && !loadInst.overlaps(accessPath: self)
      } && !analyzedInstructions.stores
      .contains { storeInst in
        storeInst.mayWrite(toAddress: storeAddr, aliasAnalysis)
        && !storeInst.storesTo(self)
      }
  }
}
