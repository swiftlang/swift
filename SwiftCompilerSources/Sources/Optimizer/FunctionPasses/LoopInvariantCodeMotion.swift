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

let loopInvariantCodeMotionPass = FunctionPass(name: "loop-invariant-code-motion") {
  function, context in
  for loop in context.loopTree.loops {
    optimizeTopLevelLoop(
      topLevelLoop: loop,
      context: context
    )
  }
//  context.removeTriviallyDeadInstructionsIgnoringDebugUses(in: function)
}

struct DiscoveredMovableInstructions {
  var toDelete: Set<Instruction> = []

  var loadsAndStores: [Instruction] = []
  var hoistUp: [Instruction] = []
  var sinkDown: [Instruction] = []
  var specialHoist: [Instruction] = []

  var loadAndStoreAddrs: [AccessPath] = []
}

private func optimizeTopLevelLoop(
  topLevelLoop: Loop,
  context: FunctionPassContext
) {
  var workList = getWorkList(topLevelLoop: topLevelLoop, context: context)
  defer {
    workList.deinitialize()
  }

  while let thisLoop = workList.pop() {
    var thisLoopChanged = false

    repeat {
      guard
        var movableInstructions = analyzeLoop(
          loop: thisLoop,
          context: context
        )
      else {
        return  // Encountered a loop without preheader. Return early.
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

private func analyzeLoop(
  loop: Loop,
  context: FunctionPassContext
) -> DiscoveredMovableInstructions? {
  guard let preheader = loop.preheader else {
    return nil
  }

  var movableInstructions = DiscoveredMovableInstructions()

  var readOnlyApplies = Stack<ApplyInst>(context)
  var globalInitCalls = Stack<Instruction>(context)

  var loopSideEffects = Stack<Instruction>(context)
  var loads = Stack<LoadInst>(context)
  var stores = Stack<StoreInst>(context)
  var fixLifetimes = Stack<FixLifetimeInst>(context)
  var beginAccesses = Stack<BeginAccessInst>(context)
  var fullApplies = Stack<FullApplySite>(context)
  defer {
    readOnlyApplies.deinitialize()
    globalInitCalls.deinitialize()
    loopSideEffects.deinitialize()
    loads.deinitialize()
    stores.deinitialize()
    fixLifetimes.deinitialize()
    beginAccesses.deinitialize()
    fullApplies.deinitialize()
  }

  // The stack data structure doesn't keep track of element count. We need to maintain those counts manually.
  var readOnlyAppliesCount = 0
  var loopSideEffectCount = 0
  var loadsCount = 0

  var hasOtherMemReadingInsts = false

  for bb in loop.basicBlocks {
    var blockSideEffects = Stack<Instruction>(context)
    defer {
      blockSideEffects.deinitialize()
    }

    for inst in bb.instructions {
      if hasOwnershipOperandsOrResults(inst: inst) {
        if checkSideEffects(
          inst: inst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        ) {
          loopSideEffectCount += 1
        }

        guard let fullApply = inst as? FullApplySite else { continue }

        fullApplies.push(fullApply)
        
        continue
      }

      switch inst {
      case let fixLifetimeInst as FixLifetimeInst:
        if fixLifetimeInst.parentBlock.dominates(preheader, context.dominatorTree) {
          fixLifetimes.push(fixLifetimeInst)
        }
      case let loadInst as LoadInst:
        loads.push(loadInst)
        loadsCount += 1
        movableInstructions.loadsAndStores.append(loadInst)
      case let storeInst as StoreInst:
        switch storeInst.storeOwnership {
        case .assign, .initialize:
          continue  // TODO: Add support
        case .unqualified, .trivial:
          break
        }
        stores.push(storeInst)
        movableInstructions.loadsAndStores.append(storeInst)
        if checkSideEffects(
          inst: storeInst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        ) {
          loopSideEffectCount += 1
        }
      case let beginAccessInst as BeginAccessInst:
        beginAccesses.push(beginAccessInst)
        if checkSideEffects(
          inst: beginAccessInst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        ) {
          loopSideEffectCount += 1
        }
      case let refElementAddrInst as RefElementAddrInst:
        movableInstructions.specialHoist.append(refElementAddrInst)
      case let condFailInst as CondFailInst:
        movableInstructions.hoistUp.append(condFailInst)
        if checkSideEffects(
          inst: condFailInst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        ) {
          loopSideEffectCount += 1
        }
      case let applyInst as ApplyInst:
        if isSafeReadOnlyApply(
          applyInst: applyInst,
          calleeAnalysis: context.calleeAnalysis
        ) {
          readOnlyApplies.push(applyInst)
          readOnlyAppliesCount += 1
        } else if let callee = applyInst.referencedFunction,
          callee.isGlobalInitFunction,
          !mayConflictWithGlobalInit(
            globalInitCall: applyInst, sideEffects: blockSideEffects,
            aliasAnalysis: context.aliasAnalysis)
        {
          globalInitCalls.push(applyInst)
        }

        fallthrough
      default:
        switch inst {
        case let fullApply as FullApplySite:
          fullApplies.push(fullApply)
        case let builtinInst as BuiltinInst:
          switch builtinInst.id {
          case .Once, .OnceWithContext:
            if !mayConflictWithGlobalInit(
              globalInitCall: builtinInst, sideEffects: blockSideEffects,
              aliasAnalysis: context.aliasAnalysis)
            {
              globalInitCalls.push(builtinInst)
            }
          default: break
          }
        default: break
        }

        if checkSideEffects(
          inst: inst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        ) {
          loopSideEffectCount += 1
        }

        if canHoistUpDefault(
          inst: inst,
          loop: loop,
          context: context
        ) {
          movableInstructions.hoistUp.append(inst)
        }
      }
    }
  }

  // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
  if readOnlyAppliesCount * loopSideEffectCount < 8000 {
    for readOnlyApply in readOnlyApplies {
      if !mayWriteTo(
        readOnlyApply,
        sideEffects: loopSideEffects,
        aliasAnalysis: context.aliasAnalysis,
        calleeAnalysis: context.calleeAnalysis
      ) {
        movableInstructions.hoistUp.append(readOnlyApply)
      }
    }
  }

  // Avoid quadratic complexity in corner cases. Usually, this limit will not be exceeded.
  if loadsCount * loopSideEffectCount < 8000 {
    for load in loads {
      if !mayWriteTo(
        load,
        sideEffects: loopSideEffects,
        aliasAnalysis: context.aliasAnalysis
      ) {
        movableInstructions.hoistUp.append(load)
      }
    }
  }

  if !globalInitCalls.isEmpty {
    // Pre check for post dom tree root node MIGHT not be necessary.

    for globalInitCall in globalInitCalls {
      if !mayConflictWithGlobalInit(
        globalInitCall: globalInitCall,
        preheader: preheader,
        sideEffects: loopSideEffects,
        aliasAnalysis: context.aliasAnalysis,
        postDomTree: context.postDominatorTree
      ) {
        movableInstructions.hoistUp.append(globalInitCall)
      }
    }
  }

  if !hasOtherMemReadingInsts {
    for storeInst in stores {
      let accessPath = storeInst.destination.accessPath

      if accessPath.isLoopInvariant(loop: loop),
        isOnlyLoadedAndStored(
          accessPath: accessPath,
          storeAddr: storeInst.destination,
          sideEffects: loopSideEffects,
          loads: loads,
          stores: stores,
          aliasAnalysis: context.aliasAnalysis
        ),
        !movableInstructions.loadAndStoreAddrs.contains(accessPath),
        splitLoads(
          loads: &loads,
          storeAddr: storeInst.destination,
          movableInstructions: &movableInstructions,
          accessPath: accessPath,
          context: context
        )
      {
        movableInstructions.loadAndStoreAddrs.append(accessPath)
      }
    }
  }

  if !fixLifetimes.isEmpty {
    let sideEffectsMayRelease = loopSideEffects.contains(where: { $0.mayRelease })

    for fixLifetime in fixLifetimes {
      guard fixLifetime.operand.value.type.isAddress else { continue }

      if sideEffectsMayRelease
        || !mayWriteTo(
          fixLifetime, sideEffects: loopSideEffects, aliasAnalysis: context.aliasAnalysis)
      {
        movableInstructions.sinkDown.append(fixLifetime)
      }
    }
  }

  for beginAccessInst in beginAccesses {
    if handledEndAccess(
      beginAccessInst: beginAccessInst,
      loop: loop,
      context: context
    )
      && analyzeBeginAccess(
        beginAccessInst: beginAccessInst,
        beginAccesses: beginAccesses,
        fullApplies: fullApplies,
        sideEffects: loopSideEffects,
        aliasAnalysis: context.aliasAnalysis,
        domTree: context.dominatorTree
      )
    {
      movableInstructions.specialHoist.append(beginAccessInst)
    }
  }

  return movableInstructions
}

private func optimizeLoop(
  loop: Loop,
  movableInstructions: inout DiscoveredMovableInstructions,
  context: FunctionPassContext
) -> Bool {
  guard loop.preheader != nil else {
    return false
  }
  
  if hoistAllLoadsAndStores(
    loop: loop,
    movableInstructions: &movableInstructions,
    context: context
  ) {
    return true // TODO: This is not very legible. We should break down this function or make a separate pass out of this.
  }

  var changed = false

  changed =
    hoistInstructions(
      loop: loop,
      hoistUp: movableInstructions.hoistUp,
      context: context
    ) || changed

  changed =
    sinkInstructions(
      loop: loop,
      sinkDown: movableInstructions.sinkDown,
      context: context
    ) || changed

  changed =
    hoistSpecialInstruction(
      loop: loop,
      specialInsts: movableInstructions.specialHoist,
      context: context
    ) || changed

  return changed
}

func hoistAllLoadsAndStores(
  loop: Loop,
  movableInstructions: inout DiscoveredMovableInstructions,
  context: FunctionPassContext
) -> Bool {
  var changed = false
  for accessPath in movableInstructions.loadAndStoreAddrs {
    changed = hoistLoadsAndStores(
      loop: loop,
      loadsAndStores: &movableInstructions.loadsAndStores,
      accessPath: accessPath,
      context: context
    ) || changed
  }
  
  guard !movableInstructions.toDelete.isEmpty else {
    return changed
  }
  
  for inst in movableInstructions.toDelete {
    if inst.isTriviallyDead {
      context.erase(instruction: inst)
    }
  }
  
  return true
}

func hoistLoadsAndStores(
  loop: Loop,
  loadsAndStores: inout [Instruction],
  accessPath: AccessPath,
  context: FunctionPassContext
) -> Bool {
  let exitingAndLatchBlocks = loop.exitingAndLatchBlocks
  
  guard storesCommonlyDominateLoopExits(
    loop: loop,
    accessPath: accessPath,
    exitingBlocks: exitingAndLatchBlocks,
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
  
  for case let storeInst as StoreInst in loadsAndStores where isStore(storeInst, thatAccesses: accessPath) {
    if let srcLoadInst = storeInst.source as? LoadInst,
       isLoad(srcLoadInst, withinAccess: accessPath){
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
    
    if let storeInst = inst as? StoreInst, isStore(storeInst, thatAccesses: accessPath) {
      currentVal = storeInst.source
      loadsAndStores.removeAll(where: { $0 == storeInst })
      context.erase(instruction: storeInst)
      changed = true
      continue
    }
    
    guard let loadInst = inst as? LoadInst,
          isLoad(loadInst, withinAccess: accessPath) else {
      continue
    }
    
    let rootVal = currentVal ?? ssaUpdater.getValue(inMiddleOf: block)
  
    guard let projectedValue = projectLoadValue(
      addr: loadInst.operand.value,
      accessPath: loadInst.operand.value.accessPath,
      rootVal: rootVal,
      rootAccessPath: accessPath,
      beforeInst: loadInst,
      context: context
    ) else {
      continue
    }
    
    loadInst.replace(with: projectedValue, context)
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

func projectLoadValue(
  addr: Value,
  accessPath: AccessPath,
  rootVal: Value,
  rootAccessPath: AccessPath,
  beforeInst: Instruction,
  context: FunctionPassContext
) -> Value? {
  guard accessPath != rootAccessPath else {
    return rootVal
  }
  
  guard let projectionPath = rootAccessPath.getProjection(to: accessPath) else {
    return nil
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
      return nil
    }
    
    currPath = remainderPath
  }
  
  return currVal
}

func storesCommonlyDominateLoopExits(
  loop: Loop,
  accessPath: AccessPath,
  exitingBlocks: some Sequence<BasicBlock>,
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
  
  for use in loop.header.parentFunction.instructions.flatMap(\.operands) where use.value.accessPath == accessPath {
    if let user = use.instruction as? StoreInst {
      stores.insert(user.parentBlock)
    }
  }
  
  guard !stores.contains(loop.header) else {
    return true
  }
  
  if let preheader = loop.preheader,
     stores.contains(preheader) {
    return true
  }
  
  storesNotAlive.insert(loop.header)
  
  var changed = false
  repeat {
    changed = false
    for block in loop.basicBlocks where !storesNotAlive.contains(block) && !stores.contains(block) && block.predecessors.contains(where: { storesNotAlive.contains($0) }) {
      storesNotAlive.insert(block)
      changed = true
    }
  } while changed
  
  for exitingBlock in exitingBlocks where !exitingBlock.successors.contains(where: { $0.terminator is UnreachableInst }) && storesNotAlive.contains(exitingBlock) {
    return false
  }
  
  return true
}

func hoistInstructions(
  loop: Loop,
  hoistUp: [Instruction],
  context: FunctionPassContext
) -> Bool {
  guard let preheader = loop.preheader else {
    return false
  }

  var dominatingBlocks = getDominatingBlocks(loop: loop, context: context)
  defer {
    dominatingBlocks.deinitialize()
  }
  var changed = false

  for bb in dominatingBlocks {
    for inst in bb.instructions where hoistUp.contains(inst) {
      changed =
        hoistInstruction(
          inst: inst,
          preheader: preheader,
          loop: loop,
          context: context
        ) || changed
    }
  }

  return changed
}

private func hoistInstruction(
  inst: Instruction,
  preheader: BasicBlock,
  loop: Loop,
  context: FunctionPassContext
) -> Bool {
  // Check whether inst is loop invariant.
  guard
    (inst.operands.allSatisfy { operand in
      !loop.basicBlocks.contains(operand.value.parentBlock)
//      !loop.basicBlockSet.contains(operand.value.parentBlock)
    })
  else {
    return false
  }

  let terminator = preheader.terminator
  if inst.canHoistArraySemanticsCall(to: terminator, context) {
    inst.hoistArraySemanticsCall(before: terminator, context)
  } else {
    inst.move(before: terminator, context)
  }

  return true
}

private func sinkInstructions(
  loop: Loop,
  sinkDown: [Instruction],
  context: FunctionPassContext
) -> Bool {
  var dominatingBlocks = getDominatingBlocks(loop: loop, context: context)
  defer {
    dominatingBlocks.deinitialize()
  }
  var changed = false

  for inst in sinkDown {
    guard dominatingBlocks.contains(inst.parentBlock) else {
      continue
    }

    changed =
      sinkInstruction(
        loop: loop,
        inst: inst,
        context: context
      ) || changed
  }

  return changed
}

private func sinkInstruction(
  loop: Loop,
  inst: Instruction,
  context: FunctionPassContext
) -> Bool {
  var isSingleExit = loop.isSingleExit
  let exitBlocks = loop.exitBlocks
  let exitingBlocks = loop.exitingBlocks
  var newExitBlocks = Stack<BasicBlock>(context)
  defer {
    newExitBlocks.deinitialize()
  }

  var changed = false

  for exitingBlock in exitingBlocks {
    for (succesorIndex, succesor) in exitingBlock.successors.enumerated().reversed()
    where !newExitBlocks.contains(succesor) && exitBlocks.contains(succesor) {

      let outsideBlock =
        context.loopTree.splitCriticalEdge(
          basicBlock: exitingBlock,
          edgeIndex: succesorIndex,
          domTree: context.dominatorTree
        ) ?? succesor

      newExitBlocks.push(outsideBlock)

      if (outsideBlock.instructions.contains { otherInst in
        inst.isIdenticalTo(otherInst)
      }) {
        isSingleExit = false
      } else if isSingleExit, let firstInstruction = outsideBlock.instructions.first {
        inst.move(before: firstInstruction, context)
      } else if let firstInstruction = outsideBlock.instructions.first {
        inst.copy(before: firstInstruction, context)
      } else {
        continue
      }

      changed = true
    }
  }

  if changed && !isSingleExit {
    context.erase(instruction: inst)
  }

  return changed
}

// TODO: Give it a better name.
private func hoistSpecialInstruction(
  loop: Loop,
  specialInsts: [Instruction],
  context: FunctionPassContext
) -> Bool {
  guard let preheader = loop.preheader else { return false }

  var changed = false

  for specialInst in specialInsts {
    if specialInst is BeginAccessInst && loop.hasNoExitBlocks {
      // TODO: We should move this as a precondition out of the loop once we remove RefElementAddrInst from here.
      continue
    }

    guard
      hoistInstruction(
        inst: specialInst,
        preheader: preheader,
        loop: loop,
        context: context
      )
    else {
      continue
    }

    // TODO: This should probably be moved to the hoistUp collection. We should keep special hoist to only BeginAccessInst.
    if let beginAccessInst = specialInst as? BeginAccessInst {
      var endAccesses = getEndAccesses(beginAccessInst: beginAccessInst, context: context)
      defer {
        endAccesses.deinitialize()
      }

      for endAccess in endAccesses {
        _ = sinkInstruction(loop: loop, inst: endAccess, context: context)
      }
    }
    
    changed = true
  }

  return changed
}

private func getDominatingBlocks(
  loop: Loop,
  context: FunctionPassContext
) -> Stack<BasicBlock> {
  var domBlocks = Stack<BasicBlock>(context)
  
  getDominatingBlocksHelper(
    bb: loop.header,
    exitingAndLatchBBs: loop.exitingAndLatchBlocks,
    domBlocks: &domBlocks,
    domTree: context.dominatorTree
  )
  
  return domBlocks
}

private func getDominatingBlocksHelper(
  bb: BasicBlock,
  exitingAndLatchBBs: some Sequence<BasicBlock>,
  domBlocks: inout Stack<BasicBlock>,
  domTree: DominatorTree
) {
  guard exitingAndLatchBBs.allSatisfy({ exitBlock in
    return bb.dominates(exitBlock, domTree)
  }) else {
    return
  }
  
  domBlocks.push(bb)
  
  for child in domTree.getChildren(of: bb) {
    getDominatingBlocksHelper(
      bb: child,
      exitingAndLatchBBs: exitingAndLatchBBs,
      domBlocks: &domBlocks,
      domTree: domTree
    )
  }
}

/// Returns `true` if `inst` may have side effects.
private func checkSideEffects(
  inst: Instruction,
  loopSideEffects: inout Stack<Instruction>,
  blockSideEffects: inout Stack<Instruction>,
  hasOtherMemReadingInsts: inout Bool
) -> Bool {
  if inst.mayHaveSideEffects {
    loopSideEffects.push(inst)
    blockSideEffects.push(inst)
    return true
  } else if inst.mayReadFromMemory {
    hasOtherMemReadingInsts = true
  }

  return false
}

private func hasOwnershipOperandsOrResults(inst: Instruction) -> Bool {
  guard inst.parentFunction.hasOwnership else { return false }

  return inst.results.contains(where: { $0.ownership != .none })
    || inst.operands.contains(where: { $0.value.ownership != .none })
}

private func isSafeReadOnlyApply(
  applyInst: ApplyInst,
  calleeAnalysis: CalleeAnalysis
) -> Bool {
  guard applyInst.functionConvention.results.allSatisfy({ $0.convention == .unowned }) else {
    return false
  }

  if let callee = applyInst.referencedFunction,
     callee.hasSemanticsAttribute("array.props.isNativeTypeChecked") {
    return false
  }

  return calleeAnalysis.getSideEffects(ofApply: applyInst).isOnlyReading
}

private func canHoistUpDefault(
  inst: Instruction,
  loop: Loop,
  context: FunctionPassContext
) -> Bool {
  guard let preheader = loop.preheader else {
    return false
  }

  if inst is TermInst || inst is Allocation || inst is Deallocation {
    return false
  }

  switch inst.getArraySemanticsCallKind() {
  case .getCount, .getCapacity:
    if inst.canHoistArraySemanticsCall(to: preheader.terminator, context)
    {
      return true
    }
  case .arrayPropsIsNativeTypeChecked:
    return false
  default:
    break
  }

  if inst.memoryEffects == .noEffects {
    return true
  }

  return false
}

private func mayWriteTo(
  _ applyInst: ApplyInst,
  sideEffects: Stack<Instruction>,
  aliasAnalysis: AliasAnalysis,
  calleeAnalysis: CalleeAnalysis
) -> Bool {
  guard calleeAnalysis.getSideEffects(ofApply: applyInst).memory == .noEffects else {
    return false
  }

  for sideEffect in sideEffects {
    switch sideEffect {
    case let storeInst as StoreInst:
      if storeInst.storeOwnership == .assign
        && applyInst.mayRead(fromAddress: storeInst.destination, aliasAnalysis)
      {
        return true
      }
    case let copyAddrInst as CopyAddrInst:
      if !copyAddrInst.isInitializationOfDestination
        && applyInst.mayRead(fromAddress: copyAddrInst.destination, aliasAnalysis)
      {
        return true
      }
    case is ApplyInst, is BeginApplyInst, is TryApplyInst:
      if !calleeAnalysis.getSideEffects(ofApply: applyInst).isOnlyReading {
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

private func mayWriteTo(
  _ unaryInst: UnaryInstruction,
  sideEffects: Stack<Instruction>,
  aliasAnalysis: AliasAnalysis
) -> Bool {
  return
    sideEffects
    .contains { sideEffect in
      sideEffect.mayWrite(toAddress: unaryInst.operand.value, aliasAnalysis)
    }
}

private func handledEndAccess(beginAccessInst: BeginAccessInst, loop: Loop, context: Context)
  -> Bool
{
  var endAccesses = getEndAccesses(beginAccessInst: beginAccessInst, context: context)
  defer {
    endAccesses.deinitialize()
  }

  return !endAccesses.isEmpty
    && !endAccesses
      .contains { user in
        !loop.basicBlocks.contains(user.parentBlock)
//        !loop.basicBlockSet.contains(user.parentBlock)
      }
}

private func getEndAccesses(beginAccessInst: BeginAccessInst, context: Context) -> Stack<
  EndAccessInst
> {
  var endAccesses = Stack<EndAccessInst>(context)

  endAccesses.append(
    contentsOf: beginAccessInst.uses.compactMap { user in
      user.instruction as? EndAccessInst
    })

  return endAccesses
}

private func mayConflictWithGlobalInit(
  globalInitCall: Instruction,
  sideEffect: Instruction,
  aliasAnalysis: AliasAnalysis
) -> Bool {
  switch sideEffect {
  case let storeInst as StoreInst:
    return globalInitCall.mayReadOrWrite(address: storeInst.destinationOperand.value, aliasAnalysis)
  case let loadInst as LoadInst:
    return globalInitCall.mayWrite(toAddress: loadInst.operand.value, aliasAnalysis)
  case is CondFailInst:
    return false
  default:
    return true
  }
}

private func mayConflictWithGlobalInit(
  globalInitCall: Instruction,
  sideEffects: Stack<Instruction>,
  aliasAnalysis: AliasAnalysis
) -> Bool {
  return
    sideEffects
    .contains { sideEffect in
      mayConflictWithGlobalInit(
        globalInitCall: globalInitCall,
        sideEffect: sideEffect,
        aliasAnalysis: aliasAnalysis
      )
    }
}

private func mayConflictWithGlobalInit(
  globalInitCall: Instruction,
  preheader: BasicBlock,
  sideEffects: Stack<Instruction>,
  aliasAnalysis: AliasAnalysis,
  postDomTree: PostDominatorTree
) -> Bool {
  guard globalInitCall.parentBlock.postDominates(preheader, postDomTree) else {
    return true
  }

  return
    sideEffects
    .contains { sideEffect in
      globalInitCall.parentBlock.strictlyPostDominates(
        sideEffect.parentBlock,
        postDomTree
      )
        && mayConflictWithGlobalInit(
          globalInitCall: globalInitCall,
          sideEffect: sideEffect,
          aliasAnalysis: aliasAnalysis
        )
    }
}

private func isOnlyLoadedAndStored(
  accessPath: AccessPath,
  storeAddr: Value,
  sideEffects: Stack<Instruction>,
  loads: Stack<LoadInst>,
  stores: Stack<StoreInst>,
  aliasAnalysis: AliasAnalysis
) -> Bool {
  let a = !sideEffects
    .contains { sideEffect in
      sideEffect.mayReadOrWrite(address: storeAddr, aliasAnalysis)
        && !isStore(sideEffect, thatAccesses: accessPath)
        && !isLoad(sideEffect, withinAccess: accessPath)
    }
  
  let b = !loads
    .contains { loadInst in
      loadInst.mayRead(fromAddress: storeAddr, aliasAnalysis)
        && !isLoadWithAccessPath(loadInst, thatOverlapsAccess: accessPath)
    }
  
  let c = !stores
    .contains { storeInst in
      storeInst.mayWrite(toAddress: storeAddr, aliasAnalysis)
        && !isStore(storeInst, thatAccesses: accessPath)
    }
  
  return a && b && c
}

private func isStore(
  _ inst: Instruction,
  thatAccesses accessPath: AccessPath
) -> Bool {
  guard let storeInst = inst as? StoreInst else {
    return false
  }

  // TODO: handle StoreOwnershipQualifier::Init
  guard storeInst.storeOwnership != .initialize else {
    return false
  }

  return accessPath == storeInst.destination.accessPath
}

private func isLoad(
  _ inst: Instruction,
  withinAccess accessPath: AccessPath
) -> Bool {
  guard let loadInst = inst as? LoadInst else {
    return false
  }

  // TODO: Check if this is sufficient
  return accessPath.getProjection(to: loadInst.address.accessPath)?.isMaterializable ?? false
}

private func isLoadWithAccessPath(
  _ inst: Instruction,
  thatOverlapsAccess accessPath: AccessPath
) -> Bool {
  guard let loadInst = inst as? LoadInst,
    loadInst.loadOwnership != .take  // TODO: handle LoadOwnershipQualifier::Take
  else {
    return false
  }
  
  return accessPath.isEqualOrContains(loadInst.operand.value.accessPath) || loadInst.operand.value.accessPath.isEqualOrContains(accessPath)
}

private func splitLoads(
  loads: inout Stack<LoadInst>,
  storeAddr: Value,
  movableInstructions: inout DiscoveredMovableInstructions,
  accessPath: AccessPath,
  context: FunctionPassContext
) -> Bool {
  var tmpLoads = Stack<LoadInst>(context)
  defer { tmpLoads.deinitialize() }
  var splitCounter = 0

  // TODO: Is the iterator created at the beggining of the loop immutable?
  while let loadInst = loads.pop() {
    guard splitCounter <= 6 else {
      tmpLoads.push(loadInst)
      loads.append(contentsOf: tmpLoads)
      return false
    }

    guard !movableInstructions.toDelete.contains(loadInst),
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

    if let splitLoads: [LoadInst] = loadInst.trySplit(alongPath: accessPath.projectionPath, context) {
      splitCounter += splitLoads.count
      movableInstructions.loadsAndStores.replace([loadInst], with: splitLoads)
      tmpLoads.append(contentsOf: splitLoads)
    }
  }
  
  loads.append(contentsOf: tmpLoads)

  return true
}

private func analyzeBeginAccess(
  beginAccessInst: BeginAccessInst,
  beginAccesses: Stack<BeginAccessInst>,
  fullApplies: Stack<FullApplySite>,
  sideEffects: Stack<Instruction>,
  aliasAnalysis: AliasAnalysis,
  domTree: DominatorTree
) -> Bool {
  let areBeginAccessesSafe =
    beginAccesses
    .allSatisfy { otherBeginAccessInst in
      guard beginAccessInst != otherBeginAccessInst else { return true }

      return beginAccessInst.accessPath.isDistinct(from: otherBeginAccessInst.accessPath)
    }

  guard areBeginAccessesSafe else { return false }

  for fullApplyInst in fullApplies {
    guard
      beginAccessInst.mayWriteToMemory
        ? fullApplyInst.mayReadOrWrite(
          address: beginAccessInst.address,
          aliasAnalysis
        )
        : fullApplyInst.mayWrite(
          toAddress: beginAccessInst.address,
          aliasAnalysis
        )
    else {
      continue
    }

    if !isCoveredByScope(
      beginAccessInst: beginAccessInst,
      otherInst: fullApplyInst,
      domTree: domTree
    ) {
      return false
    }
  }

  switch beginAccessInst.accessPath.base {
  case .class, .global:
    for sideEffect in sideEffects {
      guard sideEffect.mayRelease else {
        continue
      }

      if !isCoveredByScope(
        beginAccessInst: beginAccessInst,
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

private func isCoveredByScope(
  beginAccessInst: BeginAccessInst,
  otherInst: Instruction,
  domTree: DominatorTree
) -> Bool {
  return beginAccessInst.parentBlock.strictlyDominates(
    otherInst.parentBlock,
    domTree
  )
    && beginAccessInst.endAccessInstructions
      .allSatisfy { endAccessInst in
        otherInst.parentBlock.dominates(endAccessInst.parentBlock, domTree)
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
}
