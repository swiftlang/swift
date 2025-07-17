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

let loopInvariantCodeMotionPass = FunctionPass(name: "loop-invariant-code-motion") { function, context in
  for loop in context.loopTree.loops {
    optimizeTopLevelLoop(
      topLevelLoop: loop,
      runsOnHighLevelSil: true, // TODO: Make a parameter.
      context: context
    )
  }
}

struct DiscoveredMovableInstructions {
  var toDelete: InstructionSet
  
  var loadsAndStores: Stack<Instruction>
  var hoistUp: Stack<Instruction>
  var sinkDown: Stack<Instruction>
  var specialHoist: Stack<Instruction>
  
  var loadAndStoreAddrs: Stack<AccessPath>
  
  init(context: Context) {
    self.toDelete = InstructionSet(context)
    
    self.loadsAndStores = Stack(context)
    self.hoistUp = Stack(context)
    self.sinkDown = Stack(context)
    self.specialHoist = Stack(context)
    
    self.loadAndStoreAddrs = Stack(context)
  }
  
  mutating func deinitialize() {
    toDelete.deinitialize()
    
    loadsAndStores.deinitialize()
    hoistUp.deinitialize()
    sinkDown.deinitialize()
    specialHoist.deinitialize()
    
    loadAndStoreAddrs.deinitialize()
  }
}

private func optimizeTopLevelLoop(
  topLevelLoop: Loop,
  runsOnHighLevelSil: Bool,
  context: FunctionPassContext,
) {
  var workList = getWorkList(topLevelLoop: topLevelLoop, context: context)
  defer {
    workList.deinitialize()
  }
  
  while let thisLoop = workList.pop() {
    var thisLoopChanged = false
    
    repeat {
      guard let movableInstructions = analyzeLoop(
        loop: thisLoop,
        runsOnHighLevelSil: runsOnHighLevelSil,
        context: context
      ) else {
        return // Encountered a loop without preheader. Return early.
      }
      
      thisLoopChanged = optimizeLoop(loop: thisLoop, movableInstructions: movableInstructions)
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
    workList.deinitialize()
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
  runsOnHighLevelSil: Bool,
  context: FunctionPassContext
) -> DiscoveredMovableInstructions? {
  guard let preheader = loop.preheader else {
    return nil
  }
  
  var movableInstructions = DiscoveredMovableInstructions(context: context)
  defer {
    movableInstructions.deinitialize()
  }
  
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
      }
      
      switch inst {
      case let fixLifetimeInst as FixLifetimeInst:
        if fixLifetimeInst.parentBlock.dominates(preheader, context.dominatorTree) {
          fixLifetimes.push(fixLifetimeInst)
        }
      case let loadInst as LoadInst:
        loads.push(loadInst)
        loadsCount += 1
        movableInstructions.loadsAndStores.push(loadInst)
      case let storeInst as StoreInst:
        switch storeInst.storeOwnership {
        case .assign, .initialize:
          continue // TODO: Add support
        case .unqualified, .trivial:
          break
        }
        stores.push(storeInst)
        movableInstructions.loadsAndStores.push(storeInst)
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
        movableInstructions.specialHoist.push(refElementAddrInst)
      case let condFailInst as CondFailInst:
        movableInstructions.hoistUp.push(condFailInst)
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
          runsOnHighLevelSil: runsOnHighLevelSil,
          calleeAnalysis: context.calleeAnalysis
        ) {
          readOnlyApplies.push(applyInst)
        } else if let callee = applyInst.referencedFunction,
                  callee.isGlobalInitFunction,
                  !mayConflictWithGlobalInit(globalInitCall: applyInst, sideEffects: blockSideEffects, aliasAnalysis: context.aliasAnalysis){
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
            if !mayConflictWithGlobalInit(globalInitCall: builtinInst, sideEffects: blockSideEffects, aliasAnalysis: context.aliasAnalysis) {
              globalInitCalls.push(builtinInst)
            }
          default: break;
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
          runsOnHighLevelSil: runsOnHighLevelSil,
          domTree: context.dominatorTree
        ) {
          movableInstructions.hoistUp.push(inst)
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
        movableInstructions.hoistUp.push(readOnlyApply)
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
        movableInstructions.hoistUp.push(load)
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
        movableInstructions.hoistUp.push(globalInitCall)
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
          loads: loads,
          storeAddr: storeInst.destination,
          movableInstructions: movableInstructions,
          accessPath: accessPath,
          context: context
         ) {
        movableInstructions.loadAndStoreAddrs.push(accessPath)
      }
    }
  }
  
  if !fixLifetimes.isEmpty {
    let sideEffectsMayRelease = loopSideEffects.contains(where: { $0.mayRelease })
    
    for fixLifetime in fixLifetimes {
      guard fixLifetime.operand.value.type.isAddress else { continue }
      
      if sideEffectsMayRelease || !mayWriteTo(fixLifetime, sideEffects: loopSideEffects, aliasAnalysis: context.aliasAnalysis) {
        movableInstructions.sinkDown.push(fixLifetime)
      }
    }
  }
  
  for beginAccessInst in beginAccesses {
    if handledEndAccess(
      beginAccessInst: beginAccessInst,
      loop: loop,
      context: context
    ) && analyzeBeginAccess(
          beginAccessInst: beginAccessInst,
          beginAccesses: beginAccesses,
          fullApplies: fullApplies,
          sideEffects: loopSideEffects,
          aliasAnalysis: context.aliasAnalysis,
          domTree: context.dominatorTree
        ){
      movableInstructions.specialHoist.push(beginAccessInst)
    }
  }
  
  return movableInstructions
}

private func optimizeLoop(
  loop: Loop,
  movableInstructions: DiscoveredMovableInstructions
) -> Bool {
  // TODO: Optimize this loop
  return false
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
  
  return inst.results.contains(where: { $0.ownership != .none }) ||
         inst.operands.contains(where: { $0.value.ownership != .none })
}

private func isSafeReadOnlyApply(
  applyInst: ApplyInst,
  runsOnHighLevelSil: Bool,
  calleeAnalysis: CalleeAnalysis
) -> Bool {
  guard applyInst.functionConvention.results.allSatisfy({ $0.convention == .unowned }) else {
    return false
  }
  
  if runsOnHighLevelSil,
     let callee = applyInst.referencedFunction,
     callee.hasSemanticsAttribute("array.props.isNativeTypeChecked") {
    return false
  }
  
  return calleeAnalysis.getSideEffects(ofApply: applyInst).isOnlyReading
}

private func canHoistUpDefault(
  inst: Instruction,
  loop: Loop,
  runsOnHighLevelSil: Bool,
  domTree: DominatorTree
) -> Bool {
  guard let preheader = loop.preheader else {
    return false
  }
  
  if inst is TermInst || inst is Allocation || inst is Deallocation {
    return false
  }
  
  switch ArraySemanticsCall.getArraySemanticsCallKind(inst: inst) {
  case .getCount, .getCapacity:
    if runsOnHighLevelSil && ArraySemanticsCall.canHoist(inst: inst, to: preheader.terminator, domTree: domTree) {
      return true
    }
  case .arrayPropsIsNativeTypeChecked:
    if runsOnHighLevelSil {
      return false
    }
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
      if storeInst.storeOwnership == .assign &&
         applyInst.mayRead(fromAddress: storeInst.destination, aliasAnalysis) {
        return true;
      }
    case let copyAddrInst as CopyAddrInst:
      if !copyAddrInst.isInitializationOfDestination &&
          applyInst.mayRead(fromAddress: copyAddrInst.destination, aliasAnalysis) {
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
  return sideEffects
    .contains { sideEffect in
      sideEffect.mayWrite(toAddress: unaryInst.operand.value, aliasAnalysis)
    }
}

private func handledEndAccess(beginAccessInst: BeginAccessInst, loop: Loop, context: Context) -> Bool {
  let endAccesses = getEndAccesses(beginAccessInst: beginAccessInst, context: context)
  
  return !endAccesses.isEmpty && !endAccesses
    .contains { user in
      !loop.basicBlockSet.contains(user.parentBlock)
    }
}

private func getEndAccesses(beginAccessInst: BeginAccessInst, context: Context) -> Stack<EndAccessInst> {
  var endAccesses = Stack<EndAccessInst>(context)
  defer {
    endAccesses.deinitialize()
  }
  
  endAccesses.append(contentsOf: beginAccessInst.uses.compactMap { user in
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
  return sideEffects
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
  
  return sideEffects
    .contains { sideEffect in
      globalInitCall.parentBlock.strictlyPostDominates(
        sideEffect.parentBlock,
        postDomTree
      ) && mayConflictWithGlobalInit(
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
  return !sideEffects
    .contains { sideEffect in
      sideEffect.mayReadOrWrite(address: storeAddr, aliasAnalysis) &&
      getStore(sideEffect, ifAccesses: accessPath) == nil &&
      getLoadWithAccessPath(sideEffect, ifOverlapsAccess: accessPath) == nil
    } && !loads
    .contains { loadInst in
      loadInst.mayRead(fromAddress: storeAddr, aliasAnalysis) &&
      getLoadWithAccessPath(loadInst, ifOverlapsAccess: accessPath) == nil
    } && !stores
    .contains { storeInst in
      storeInst.mayWrite(toAddress: storeAddr, aliasAnalysis) &&
      getStore(storeInst, ifAccesses: accessPath) == nil
    }
}

typealias LoadWithAccessPath = (loadInst: LoadInst, accessPath: AccessPath)

private func getStore(
  _ inst: Instruction,
  ifAccesses accessPath: AccessPath
) -> StoreInst? {
  guard let storeInst = inst as? StoreInst else {
    return nil
  }
  
  // TODO: handle StoreOwnershipQualifier::Init
  guard storeInst.storeOwnership != .initialize else {
    return nil
  }
  
  return accessPath == storeInst.destination.accessPath ? storeInst : nil
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

private func getLoadWithAccessPath(
  _ inst: Instruction,
  ifOverlapsAccess accessPath: AccessPath
) -> LoadWithAccessPath? {
  guard let loadInst = inst as? LoadInst,
        loadInst.loadOwnership != .take, // TODO: handle LoadOwnershipQualifier::Take
        !loadInst.operand.value.accessPath.isEqualOrContains(accessPath),
        !accessPath.isEqualOrContains(loadInst.operand.value.accessPath) else {
    return nil
  }
  
  return (loadInst, accessPath)
}

private func splitLoads(
  loads: Stack<LoadInst>,
  storeAddr: Value,
  movableInstructions: DiscoveredMovableInstructions,
  accessPath: AccessPath,
  context: FunctionPassContext
) -> Bool {
  // TODO: Is the iterator created at the beggining of the loop immutable?
  for loadInst in loads {
    guard !movableInstructions.toDelete.contains(loadInst),
          loadInst.mayRead(fromAddress: storeAddr, context.aliasAnalysis),
          !accessPath.isEqualOrContains(loadInst.operand.value.accessPath) else {
      continue
    }
    
    // TODO: More stuff
    
    loadInst.trySplit(context)
    
    // TODO: Rest of logic
  }
  
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
  let areBeginAccessesSafe = beginAccesses
    .allSatisfy { otherBeginAccessInst in
      guard beginAccessInst != otherBeginAccessInst else { return true }
      
      return beginAccessInst.accessPath.isDistinct(from: otherBeginAccessInst.accessPath)
    }
  
  guard areBeginAccessesSafe else { return false }
  
  for fullApplyInst in fullApplies {
    guard beginAccessInst.mayWriteToMemory ? fullApplyInst.mayReadOrWrite(
      address: beginAccessInst.address,
      aliasAnalysis
    ) : fullApplyInst.mayWrite(
      toAddress: beginAccessInst.address,
      aliasAnalysis
    ) else {
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
  return beginAccessInst.parentBlock.dominates(
    otherInst.parentBlock,
    domTree
  ) && beginAccessInst.endAccessInstructions
    .allSatisfy { endAccessInst in
      otherInst.parentBlock.dominates(endAccessInst.parentBlock, domTree)
    }
}

private extension AccessPath {
  func isLoopInvariant(loop: Loop) -> Bool {
    switch base {
    case .box(let inst as Instruction), .class(let inst as Instruction), .index(let inst as Instruction),
         .pointer(let inst as Instruction), .stack(let inst as Instruction), .storeBorrow(let inst as Instruction),
         .tail(let inst as Instruction):
      if loop.basicBlockSet.contains(inst.parentBlock) {
        return false
      }
    case .global, .argument:
      break
    case .yield(let beginApplyResult):
      if loop.basicBlockSet.contains(beginApplyResult.parentBlock) {
        return false
      }
    case .unidentified:
      return false
    }
    
    return !projectionPath.isConstant
  }
}
