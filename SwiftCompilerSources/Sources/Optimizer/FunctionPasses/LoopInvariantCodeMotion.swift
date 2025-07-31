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

let loopInvariantCodeMotionPass = FunctionPass(
  name: "loop-invariant-code-motion"
) { function, context in
  for loop in context.loopTree.loops {
    optimizeTopLevelLoop(
      topLevelLoop: loop,
      context: context
    )
  }
}

private func optimizeTopLevelLoop(
  topLevelLoop: Loop,
  context: FunctionPassContext
) {
  var workList = getWorkList(topLevelLoop: topLevelLoop, context: context)
  defer { workList.deinitialize() }

  while let thisLoop = workList.pop() {
    var thisLoopChanged = false

    repeat {
      guard
        var movableInstructions = analyzeLoop(
          loop: thisLoop,
          context: context
        )
      else {
        return // Encountered a loop without preheader. Return early.
      }

      thisLoopChanged = optimizeLoop(
        loop: thisLoop,
        movableInstructions: &movableInstructions,
        context: context
      )
    } while thisLoopChanged
  }
}

private func getWorkList(topLevelLoop: Loop, context: Context) -> Stack<Loop> {
  var tmp1 = Stack<Loop>(context)
  var tmp2 = Stack<Loop>(context)
  var workList = Stack<Loop>(context)
  defer {
    tmp1.deinitialize()
    tmp2.deinitialize()
  }

  tmp1.push(topLevelLoop)

  while !tmp1.isEmpty || !tmp2.isEmpty {
    while let loop = tmp2.pop() {
      workList.push(loop)
    }

    while let loop = tmp1.pop() {
      tmp2.push(loop)
      tmp1.append(contentsOf: loop.innerLoops)
    }
  }

  return workList
}

struct MovableInstructions {
  var loadAndStoreAddrs: [AccessPath] = []
  
  var loadsAndStores: [Instruction] = []
  var hoistUp: [Instruction] = []
  var sinkDown: [Instruction] = []
  var specialHoist: [Instruction] = []
}

struct AnalyzedInstructions {
  private(set) var globalInitCalls: Stack<Instruction>
  private(set) var loopSideEffects: Stack<Instruction>
  
  private(set) var readOnlyApplies: Stack<ApplyInst>
  private(set) var loads: Stack<LoadInst>
  private(set) var stores: Stack<StoreInst>
  private(set) var fixLifetimes: Stack<FixLifetimeInst>
  private(set) var beginAccesses: Stack<BeginAccessInst>
  private(set) var fullApplies: Stack<FullApplySite>
  
  private(set) var readOnlyAppliesCount = 0
  private(set) var loopSideEffectCount = 0
  private(set) var loadsCount = 0
  
  init (_ context: FunctionPassContext) {
    self.globalInitCalls = Stack<Instruction>(context)
    self.loopSideEffects = Stack<Instruction>(context)
    
    self.readOnlyApplies = Stack<ApplyInst>(context)
    self.loads = Stack<LoadInst>(context)
    self.stores = Stack<StoreInst>(context)
    self.fixLifetimes = Stack<FixLifetimeInst>(context)
    self.beginAccesses = Stack<BeginAccessInst>(context)
    self.fullApplies = Stack<FullApplySite>(context)
  }
  
  mutating func appendGlobalInitCall(_ inst: Instruction) { globalInitCalls.append(inst) }
  
  mutating func appendSideEffect(_ inst: Instruction) {
    loopSideEffects.append(inst)
    loopSideEffectCount += 1
  }
  
  mutating func append(_ applyInst: ApplyInst) {
    readOnlyApplies.append(applyInst)
    readOnlyAppliesCount += 1
  }
  
  mutating func append(_ loadInst: LoadInst) {
    loads.append(loadInst)
    loadsCount += 1
  }
  
  mutating func popLoad() -> LoadInst? {
    guard !loads.isEmpty else {
      return nil
    }
    
    loadsCount -= 1
    return loads.pop()
  }
  
  mutating func append(newLoads: Stack<LoadInst>) {
    for load in newLoads {
      append(load)
    }
  }
  
  mutating func append(_ storeInst: StoreInst) { stores.append(storeInst) }
  mutating func append(_ fixLifetime: FixLifetimeInst) { fixLifetimes.append(fixLifetime) }
  mutating func append(_ beginAccess: BeginAccessInst) { beginAccesses.append(beginAccess) }
  mutating func append(_ fullApply: FullApplySite) { fullApplies.append(fullApply) }
  
  mutating func deinitialize() {
    readOnlyApplies.deinitialize()
    globalInitCalls.deinitialize()
    loopSideEffects.deinitialize()
    loads.deinitialize()
    stores.deinitialize()
    fixLifetimes.deinitialize()
    beginAccesses.deinitialize()
    fullApplies.deinitialize()
  }
}

private func analyzeLoop(
  loop: Loop,
  context: FunctionPassContext
) -> MovableInstructions? {
  guard let preheader = loop.preheader else {
    return nil
  }

  var movableInstructions = MovableInstructions()
  var analyzedInstructions = AnalyzedInstructions(context)
  defer { analyzedInstructions.deinitialize() }

  var hasOtherMemReadingInsts = false

  for bb in loop.basicBlocks {
    var blockSideEffects = Stack<Instruction>(context)
    defer { blockSideEffects.deinitialize() }

    for inst in bb.instructions {
      if inst.hasOwnershipOperandsOrResults {
        inst.analyzeSideEffects(
          analyzedInstructions: &analyzedInstructions,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
        
        if let fullApply = inst as? FullApplySite {
          analyzedInstructions.append(fullApply)
        }
        
        continue
      }

      switch inst {
      case let fixLifetimeInst as FixLifetimeInst where fixLifetimeInst.parentBlock.dominates(preheader, context.dominatorTree):
        analyzedInstructions.append(fixLifetimeInst)
      case let loadInst as LoadInst:
        analyzedInstructions.append(loadInst)
        movableInstructions.loadsAndStores.append(loadInst)
      case let storeInst as StoreInst:
        switch storeInst.storeOwnership {
        case .assign, .initialize:
          continue  // TODO: Add support
        case .unqualified, .trivial:
          break
        }
        analyzedInstructions.append(storeInst)
        movableInstructions.loadsAndStores.append(storeInst)
        storeInst.analyzeSideEffects(
          analyzedInstructions: &analyzedInstructions,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let beginAccessInst as BeginAccessInst:
        analyzedInstructions.append(beginAccessInst)
        beginAccessInst.analyzeSideEffects(
          analyzedInstructions: &analyzedInstructions,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let refElementAddrInst as RefElementAddrInst:
        movableInstructions.specialHoist.append(refElementAddrInst)
      case let condFailInst as CondFailInst:
        movableInstructions.hoistUp.append(condFailInst)
        condFailInst.analyzeSideEffects(
          analyzedInstructions: &analyzedInstructions,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let applyInst as ApplyInst:
        if applyInst.isSafeReadOnlyApply(
          calleeAnalysis: context.calleeAnalysis
        ) {
          analyzedInstructions.append(applyInst)
        } else if let callee = applyInst.referencedFunction,
          callee.isGlobalInitFunction,
          !applyInst.globalInitMayConflictWith(
            sideEffects: blockSideEffects,
            aliasAnalysis: context.aliasAnalysis
          ) {
          analyzedInstructions.appendGlobalInitCall(applyInst)
        }

        fallthrough
      default:
        switch inst {
        case let fullApply as FullApplySite:
          analyzedInstructions.append(fullApply)
        case let builtinInst as BuiltinInst:
          switch builtinInst.id {
          case .Once, .OnceWithContext:
            if !builtinInst.globalInitMayConflictWith(
              sideEffects: blockSideEffects,
              aliasAnalysis: context.aliasAnalysis
            ) {
              analyzedInstructions.appendGlobalInitCall(builtinInst)
            }
          default: break
          }
        default: break
        }

        inst.analyzeSideEffects(
          analyzedInstructions: &analyzedInstructions,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )

        if inst.canBeHoisted(
          outOf: loop,
          context: context
        ) {
          movableInstructions.hoistUp.append(inst)
        }
      }
    }
  }

  // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
  if analyzedInstructions.readOnlyAppliesCount * analyzedInstructions.loopSideEffectCount < 8000 {
    for readOnlyApply in analyzedInstructions.readOnlyApplies {
      if !readOnlyApply.mayWriteTo(
        sideEffects: analyzedInstructions.loopSideEffects,
        aliasAnalysis: context.aliasAnalysis,
        calleeAnalysis: context.calleeAnalysis
      ) {
        movableInstructions.hoistUp.append(readOnlyApply)
      }
    }
  }

  // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
  if analyzedInstructions.loadsCount * analyzedInstructions.loopSideEffectCount < 8000 {
    for load in analyzedInstructions.loads {
      if !load.mayWriteTo(
        sideEffects: analyzedInstructions.loopSideEffects,
        aliasAnalysis: context.aliasAnalysis
      ) {
        movableInstructions.hoistUp.append(load)
      }
    }
  }

  if !analyzedInstructions.globalInitCalls.isEmpty {
    for globalInitCall in analyzedInstructions.globalInitCalls {
      if !globalInitCall.globalInitMayConflictWith(
        preheader: preheader,
        sideEffects: analyzedInstructions.loopSideEffects,
        aliasAnalysis: context.aliasAnalysis,
        postDomTree: context.postDominatorTree
      ) {
        movableInstructions.hoistUp.append(globalInitCall)
      }
    }
  }

  if !hasOtherMemReadingInsts {
    for storeInst in analyzedInstructions.stores {
      let accessPath = storeInst.destination.accessPath
      if accessPath.isLoopInvariant(loop: loop),
         accessPath.isOnlyLoadedAndStored(
          storeAddr: storeInst.destination,
          analyzedInstructions: analyzedInstructions,
          aliasAnalysis: context.aliasAnalysis
         ),
         !movableInstructions.loadAndStoreAddrs.contains(accessPath),
         movableInstructions.splitLoads(
           analyzedInstructions: &analyzedInstructions,
           storeAddr: storeInst.destination,
           accessPath: accessPath,
           context: context
         ) {
        movableInstructions.loadAndStoreAddrs.append(accessPath)
      }
    }
  }

  if !analyzedInstructions.fixLifetimes.isEmpty {
    let sideEffectsMayRelease = analyzedInstructions.loopSideEffects.contains(where: { $0.mayRelease })

    for fixLifetime in analyzedInstructions.fixLifetimes {
      guard fixLifetime.operand.value.type.isAddress else { continue }

      if sideEffectsMayRelease || !fixLifetime.mayWriteTo(
        sideEffects: analyzedInstructions.loopSideEffects,
        aliasAnalysis: context.aliasAnalysis
      ) {
        movableInstructions.sinkDown.append(fixLifetime)
      }
    }
  }

  for beginAccessInst in analyzedInstructions.beginAccesses {
    if beginAccessInst.canBeHoisted(
      outOf: loop,
      analyzedInstructions: analyzedInstructions,
      aliasAnalysis: context.aliasAnalysis,
      domTree: context.dominatorTree
    ) {
      movableInstructions.specialHoist.append(beginAccessInst)
    }
  }

  return movableInstructions
}

private func optimizeLoop(
  loop: Loop,
  movableInstructions: inout MovableInstructions,
  context: FunctionPassContext
) -> Bool {
  if movableInstructions.hoistAllLoadsAndStores(
    outOf: loop,
    context: context
  ) {
    return true
  }

  var changed = false

  changed = movableInstructions.hoistInstructions(
      loop: loop,
      context: context
    ) || changed

  changed = movableInstructions.sinkInstructions(
      loop: loop,
      context: context
    ) || changed

  changed = movableInstructions.hoistWithSinkScopedInstructions(
      loop: loop,
      context: context
    ) || changed

  return changed
}

extension MovableInstructions {
  mutating func splitLoads(
    analyzedInstructions: inout AnalyzedInstructions,
    storeAddr: Value,
    accessPath: AccessPath,
    context: FunctionPassContext
  ) -> Bool {
    var tmpLoads = Stack<LoadInst>(context)
    defer {
      analyzedInstructions.append(newLoads: tmpLoads)
      tmpLoads.deinitialize()
    }
    var splitCounter = 0

    while let loadInst = analyzedInstructions.popLoad() {
      guard splitCounter <= 6 else {
        tmpLoads.push(loadInst)
        return false
      }

      guard !loadInst.isDeleted,
        loadInst.mayRead(fromAddress: storeAddr, context.aliasAnalysis),
        !accessPath.isEqualOrContains(loadInst.operand.value.accessPath)
      else {
        tmpLoads.push(loadInst)
        continue
      }

      let loadAccessPath = loadInst.operand.value.accessPath
      guard !accessPath.isEqualOrContains(loadAccessPath) else {
        tmpLoads.push(loadInst)
        continue
      }

      if let splitLoads = loadInst.trySplit(alongPath: accessPath.projectionPath, context) {
        splitCounter += splitLoads.count
        loadsAndStores.replace([loadInst], with: splitLoads)
        tmpLoads.append(contentsOf: splitLoads)
      }
    }

    return true
  }
  
  mutating func hoistAllLoadsAndStores(
    outOf loop: Loop,
    context: FunctionPassContext
  ) -> Bool {
    var changed = false
    for accessPath in loadAndStoreAddrs {
      changed = hoistAndSinkLoadsAndStores(
        outOf: loop,
        accessPath: accessPath,
        context: context
      ) || changed
    }
    
    return changed
  }
  
  mutating func hoistInstructions(
    loop: Loop,
    context: FunctionPassContext
  ) -> Bool {
    let dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(domTree: context.dominatorTree)
    var changed = false

    for bb in dominatingBlocks {
      for inst in bb.instructions where hoistUp.contains(inst) {
        changed = inst.hoist(
          outOf: loop,
          context: context
        ) || changed
      }
    }

    return changed
  }

  mutating func sinkInstructions(
    loop: Loop,
    context: FunctionPassContext
  ) -> Bool {
    let dominatingBlocks = loop.getBlocksThatDominateAllExitingAndLatchBlocks(domTree: context.dominatorTree)
    var changed = false

    for inst in sinkDown where dominatingBlocks.contains(inst.parentBlock) {
      changed = inst.sink(
        outOf: loop,
        context: context
      ) || changed
    }

    return changed
  }

  mutating func hoistWithSinkScopedInstructions(
    loop: Loop,
    context: FunctionPassContext
  ) -> Bool {
    var changed = false

    for specialInst in specialHoist {
      if specialInst is BeginAccessInst && loop.hasNoExitBlocks {
        // TODO: We should move this as a precondition out of the loop once we remove RefElementAddrInst from here.
        continue
      }

      guard
        specialInst.hoist(outOf: loop, context: context)
      else {
        continue
      }

      // TODO: This should probably be moved to the hoistUp collection. We should keep special hoist to only BeginAccessInst.
      if let beginAccessInst = specialInst as? BeginAccessInst {
        for endAccess in beginAccessInst.endAccessInstructions {
          endAccess.sink(outOf: loop, context: context)
        }
      }
      
      changed = true
    }

    return changed
  }

  private mutating func hoistAndSinkLoadsAndStores(
    outOf loop: Loop,
    accessPath: AccessPath,
    context: FunctionPassContext
  ) -> Bool {
    let exitingAndLatchBlocks = loop.exitingAndLatchBlocks
    
    guard loop.storesCommonlyDominateExits(
      accessPath: accessPath,
      context: context
    ) else {
      return false
    }
    
    for exitingOrLatchBlock in exitingAndLatchBlocks {
      for (index, succesor) in exitingOrLatchBlock.successors.enumerated() where !loop.basicBlocks.contains(succesor) {
        _ = context.loopTree.splitCriticalEdge(
          basicBlock: exitingOrLatchBlock.terminator.parentBlock,
          edgeIndex: index,
          domTree: context.dominatorTree
        )
      }
    }
    
    guard let preheader = loop.preheader else {
      return false
    }
    
    var changed = false
    let builder = Builder(before: preheader.terminator, context)
    var ssaUpdater: SSAUpdater<FunctionPassContext>?
    var storeAddr: Value?
    
    for case let storeInst as StoreInst in loadsAndStores where storeInst.accesses(accessPath) {
      if let srcLoadInst = storeInst.source as? LoadInst,
         srcLoadInst.accesses(accessPath) {
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
      
      if let storeInst = inst as? StoreInst, storeInst.accesses(accessPath) {
        currentVal = storeInst.source
        loadsAndStores.removeAll(where: { $0 == storeInst })
        context.erase(instruction: storeInst)
        changed = true
        continue
      }
      
      guard let loadInst = inst as? LoadInst,
            loadInst.accesses(accessPath) else {
        continue
      }
      
      let rootVal = currentVal ?? ssaUpdater.getValue(inMiddleOf: block)
    
      guard loadInst.replaceLoadWithProjection(
        rootVal: rootVal,
        rootAccessPath: accessPath,
        beforeInst: loadInst,
        context: context
      ) else {
        continue
      }
      
      loadsAndStores.removeAll(where: { $0 == loadInst })
      changed = true
    }
    
    for exitingOrLatchBlock in exitingAndLatchBlocks {
      for succesor in exitingOrLatchBlock.successors where !loop.basicBlocks.contains(succesor) {
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
    
    if initialLoad.isTriviallyDead {
      context.erase(instruction: initialLoad)
    }
    
    return changed
  }
}

extension Loop {
  fileprivate func storesCommonlyDominateExits(
    accessPath: AccessPath,
    context: FunctionPassContext
  ) -> Bool {
    var stores = BasicBlockSet(context)
    var storesNotAlive = BasicBlockSet(context)
    var uses = Stack<Instruction>(context)
    defer {
      stores.deinitialize()
      storesNotAlive.deinitialize()
      uses.deinitialize()
    }
    
    for use in header.parentFunction.instructions.flatMap(\.operands) where use.value.accessPath == accessPath {
      if let user = use.instruction as? StoreInst {
        stores.insert(user.parentBlock)
      }
    }
    
    guard !stores.contains(header) else {
      return true
    }
    
    if let preheader = preheader,
       stores.contains(preheader) {
      return true
    }
    
    storesNotAlive.insert(header)
    
    var changed = false
    repeat {
      changed = false
      for block in basicBlocks where !storesNotAlive.contains(block) && !stores.contains(block) && block.predecessors.contains(where: { storesNotAlive.contains($0) }) {
        storesNotAlive.insert(block)
        changed = true
      }
    } while changed
    
    for exitingBlock in exitingBlocks where !exitingBlock.successors.contains(where: { $0.terminator is UnreachableInst }) && storesNotAlive.contains(exitingBlock) {
      return false
    }
    
    return true
  }
}

extension Instruction {
  fileprivate func analyzeSideEffects(
    analyzedInstructions: inout AnalyzedInstructions,
    blockSideEffects: inout Stack<Instruction>,
    hasOtherMemReadingInsts: inout Bool
  ) {
    if mayHaveSideEffects {
      analyzedInstructions.appendSideEffect(self)
      blockSideEffects.append(self)
    } else if mayReadFromMemory {
      hasOtherMemReadingInsts = true
    }
  }
  
  fileprivate var hasOwnershipOperandsOrResults: Bool {
    guard parentFunction.hasOwnership else { return false }

    return results.contains(where: { $0.ownership != .none })
      || operands.contains(where: { $0.value.ownership != .none })
  }
  
  fileprivate func canBeHoisted(
    outOf loop: Loop,
    context: FunctionPassContext
  ) -> Bool {
    guard let preheader = loop.preheader else {
      return false
    }

    if self is TermInst || self is Allocation || self is Deallocation {
      return false
    }

    switch getArraySemanticsCallKind() {
    case .getCount, .getCapacity:
      if canHoistArraySemanticsCall(to: preheader.terminator, context) {
        return true
      }
    case .arrayPropsIsNativeTypeChecked:
      return false
    default:
      break
    }

    if memoryEffects == .noEffects {
      return true
    }

    return false
  }
  
  @discardableResult
  fileprivate func hoist(
    outOf loop: Loop,
    context: FunctionPassContext
  ) -> Bool {
    guard let preheader = loop.preheader,
          operands.allSatisfy({ !loop.basicBlocks.contains($0.value.parentBlock) }) else {
      return false
    }

    let terminator = preheader.terminator
    if canHoistArraySemanticsCall(to: terminator, context) {
      hoistArraySemanticsCall(before: terminator, context)
    } else {
      move(before: terminator, context)
    }
    
    if let singleValueInst = self as? SingleValueInstruction,
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
  
  @discardableResult
  fileprivate func sink(
    outOf loop: Loop,
    context: FunctionPassContext
  ) -> Bool {
    var isSingleExit = loop.isSingleExit
    let exitBlocks = loop.exitBlocks
    let exitingBlocks = loop.exitingBlocks
    var newExitBlocks = Stack<BasicBlock>(context)
    defer { newExitBlocks.deinitialize() }
    var changed = false

    for exitingBlock in exitingBlocks {
      for (succesorIndex, succesor) in exitingBlock.successors.enumerated().reversed()
      where !newExitBlocks.contains(succesor) && exitBlocks.contains(succesor) {

        let outsideBlock = context.loopTree.splitCriticalEdge(
          basicBlock: exitingBlock,
          edgeIndex: succesorIndex,
          domTree: context.dominatorTree
        ) ?? succesor

        newExitBlocks.push(outsideBlock)

        if outsideBlock.instructions.contains(where: { isIdenticalTo($0) }) {
          isSingleExit = false
        } else if isSingleExit, let firstInstruction = outsideBlock.instructions.first {
          move(before: firstInstruction, context)
        } else if let firstInstruction = outsideBlock.instructions.first {
          copy(before: firstInstruction, context)
        } else {
          continue
        }

        changed = true
      }
    }

    if changed && !isSingleExit {
      context.erase(instruction: self)
    }

    return changed
  }
  
  private func globalInitMayConflictWith(
    sideEffect: Instruction,
    aliasAnalysis: AliasAnalysis
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
  
  fileprivate func globalInitMayConflictWith(
    sideEffects: Stack<Instruction>,
    aliasAnalysis: AliasAnalysis
  ) -> Bool {
    return sideEffects
      .contains { sideEffect in
        globalInitMayConflictWith(
          sideEffect: sideEffect,
          aliasAnalysis: aliasAnalysis
        )
      }
  }

  fileprivate func globalInitMayConflictWith(
    preheader: BasicBlock,
    sideEffects: Stack<Instruction>,
    aliasAnalysis: AliasAnalysis,
    postDomTree: PostDominatorTree
  ) -> Bool {
    guard parentBlock.postDominates(preheader, postDomTree) else {
      return true
    }

    return sideEffects
      .contains { sideEffect in
        parentBlock.strictlyPostDominates(
          sideEffect.parentBlock,
          postDomTree
        ) && globalInitMayConflictWith(
          sideEffect: sideEffect,
          aliasAnalysis: aliasAnalysis
        )
      }
  }
}

extension UnaryInstruction {
  fileprivate func mayWriteTo(
    sideEffects: Stack<Instruction>,
    aliasAnalysis: AliasAnalysis
  ) -> Bool {
    return sideEffects
      .contains { sideEffect in
        sideEffect.mayWrite(toAddress: operand.value, aliasAnalysis)
      }
  }
}

extension StoreInst {
  fileprivate func accesses(_ accessPath: AccessPath) -> Bool {
    guard self.storeOwnership != .initialize else {
      return false
    }

    return accessPath == self.destination.accessPath
  }
}

extension LoadInst {
  fileprivate func replaceLoadWithProjection(
    rootVal: Value,
    rootAccessPath: AccessPath,
    beforeInst: Instruction,
    context: FunctionPassContext
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
  
  fileprivate func accesses(
    _ accessPath: AccessPath
  ) -> Bool {
    return accessPath.getProjection(to: self.address.accessPath)?.isMaterializable ?? false
  }
  
  fileprivate func overlaps(
    accessPath: AccessPath
  ) -> Bool {
    guard self.loadOwnership != .take else { // TODO: handle LoadOwnershipQualifier::Take
      return false
    }
    
    return accessPath.isEqualOrContains(self.operand.value.accessPath) || self.operand.value.accessPath.isEqualOrContains(accessPath)
  }
}

extension ApplyInst {
  fileprivate func isSafeReadOnlyApply(
    calleeAnalysis: CalleeAnalysis
  ) -> Bool {
    guard functionConvention.results.allSatisfy({ $0.convention == .unowned }) else {
      return false
    }

    if let callee = referencedFunction,
       callee.hasSemanticsAttribute("array.props.isNativeTypeChecked") {
      return false
    }

    return calleeAnalysis.getSideEffects(ofApply: self).isOnlyReading
  }
  
  fileprivate func mayWriteTo(
    sideEffects: Stack<Instruction>,
    aliasAnalysis: AliasAnalysis,
    calleeAnalysis: CalleeAnalysis
  ) -> Bool {
    guard calleeAnalysis.getSideEffects(ofApply: self).memory == .noEffects else {
      return false
    }

    for sideEffect in sideEffects {
      switch sideEffect {
      case let storeInst as StoreInst:
        if storeInst.storeOwnership == .assign &&
           mayRead(fromAddress: storeInst.destination, aliasAnalysis) {
          return true
        }
      case let copyAddrInst as CopyAddrInst:
        if !copyAddrInst.isInitializationOfDestination &&
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

extension BeginAccessInst {
  private func isCoveredByScope(
    otherInst: Instruction,
    domTree: DominatorTree
  ) -> Bool {
    return self.parentBlock.strictlyDominates(
      otherInst.parentBlock,
      domTree
    ) && self.endAccessInstructions
      .allSatisfy { endAccessInst in
        otherInst.parentBlock.dominates(endAccessInst.parentBlock, domTree)
      }
  }
  
  private func handlesAllEndAccesses(
    loop: Loop
  ) -> Bool {
    return !endAccessInstructions.isEmpty && !endAccessInstructions
        .contains { user in
          !loop.basicBlocks.contains(user.parentBlock)
        }
  }
  
  fileprivate func canBeHoisted(
    outOf loop: Loop,
    analyzedInstructions: AnalyzedInstructions,
    aliasAnalysis: AliasAnalysis,
    domTree: DominatorTree
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

    for fullApplyInst in analyzedInstructions.fullApplies {
      guard mayWriteToMemory
          ? fullApplyInst.mayReadOrWrite(
            address: address,
            aliasAnalysis
          ) : fullApplyInst.mayWrite(
            toAddress: address,
            aliasAnalysis
          ) else {
        continue
      }

      if !isCoveredByScope(
        otherInst: fullApplyInst,
        domTree: domTree
      ) {
        return false
      }
    }

    switch accessPath.base {
    case .class, .global:
      for sideEffect in analyzedInstructions.loopSideEffects {
        guard sideEffect.mayRelease else {
          continue
        }

        if !isCoveredByScope(
          otherInst: sideEffect,
          domTree: domTree
        ) {
          return false
        }
      }

      return true
    default:
      return true
    }
  }
}

extension AccessPath {
  fileprivate func isLoopInvariant(loop: Loop) -> Bool {
    switch base {
    case .box(let inst as Instruction), .class(let inst as Instruction),
      .index(let inst as Instruction),
      .pointer(let inst as Instruction), .stack(let inst as Instruction),
      .storeBorrow(let inst as Instruction),
      .tail(let inst as Instruction):
      if loop.basicBlocks.contains(inst.parentBlock) {
        return false
      }
    case .global, .argument:
      break
    case .yield(let beginApplyResult):
      if loop.basicBlocks.contains(beginApplyResult.parentBlock) {
        return false
      }
    case .unidentified:
      return false
    }

    return projectionPath.isConstant
  }
  
  fileprivate func isOnlyLoadedAndStored(
    storeAddr: Value,
    analyzedInstructions: AnalyzedInstructions,
    aliasAnalysis: AliasAnalysis
  ) -> Bool {
    return !analyzedInstructions.loopSideEffects
      .contains { sideEffect in
        let accessesPath: Bool
        switch sideEffect {
        case let storeInst as StoreInst:
          accessesPath = storeInst.accesses(self)
        case let loadInst as LoadInst:
          accessesPath = loadInst.accesses(self)
        default:
          accessesPath = false
        }
        
        return !accessesPath && sideEffect.mayReadOrWrite(
          address: storeAddr,
          aliasAnalysis
        )
      } && !analyzedInstructions.loads
      .contains { loadInst in
        loadInst.mayRead(fromAddress: storeAddr, aliasAnalysis)
        && !loadInst.overlaps(accessPath: self)
      } && !analyzedInstructions.stores
      .contains { storeInst in
        storeInst.mayWrite(toAddress: storeAddr, aliasAnalysis)
        && !storeInst.accesses(self)
      }
  }
}
