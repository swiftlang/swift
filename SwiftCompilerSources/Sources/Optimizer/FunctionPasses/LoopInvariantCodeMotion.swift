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
  var loadsAndStores: InstructionSet
  var hoistUp: InstructionSet
  var sinkDown: InstructionSet
  var specialHoist: InstructionSet
  
  init(context: Context) {
    self.loadsAndStores = InstructionSet(context)
    self.hoistUp = InstructionSet(context)
    self.sinkDown = InstructionSet(context)
    self.specialHoist = InstructionSet(context)
  }
  
  mutating func deinitialize() {
    loadsAndStores.deinitialize()
    hoistUp.deinitialize()
    sinkDown.deinitialize()
    specialHoist.deinitialize()
  }
}

private func optimizeTopLevelLoop(
  topLevelLoop: Loop,
  runsOnHighLevelSil: Bool,
  context: FunctionPassContext,
) {
  // FIXME: Make a recursive function that populates the stack work list. Efficiency purposes.
  var workList = [topLevelLoop]
  var i = 0
  while i < workList.count {
    let thisLoop = workList[i]
    workList.append(contentsOf: thisLoop.innerLoops)
    i += 1
  }
  
  while let thisLoop = workList.popLast() {
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
  
  var hasOtherMemReadingInsts = false
  
  for bb in loop.basicBlocks {
    var blockSideEffects = Stack<Instruction>(context)
    defer {
      blockSideEffects.deinitialize()
    }
    
    for inst in bb.instructions {
      if hasOwnershipOperandsOrResults(inst: inst) {
        checkSideEffects(
          inst: inst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
        
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
        movableInstructions.loadsAndStores.insert(loadInst)
      case let storeInst as StoreInst:
        switch storeInst.storeOwnership {
        case .assign, .initialize:
          continue // TODO: Add support
        case .unqualified, .trivial:
          break
        }
        stores.push(storeInst)
        movableInstructions.loadsAndStores.insert(storeInst)
        checkSideEffects(
          inst: storeInst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let beginAccessInst as BeginAccessInst:
        beginAccesses.push(beginAccessInst)
        checkSideEffects(
          inst: beginAccessInst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let refElementAddrInst as RefElementAddrInst:
        movableInstructions.specialHoist.insert(refElementAddrInst)
      case let condFailInst as CondFailInst:
        movableInstructions.hoistUp.insert(condFailInst)
        checkSideEffects(
          inst: condFailInst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
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
        
        checkSideEffects(
          inst: inst,
          loopSideEffects: &loopSideEffects,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
        
        if canHoistUpDefault(inst: inst, loop: loop, domTree: context.dominatorTree) {
          movableInstructions.hoistUp.insert(inst)
        }
      }
    }
  }
  
  // TODO: Check for size to avoid quadratic complexity
  for readOnlyApply in readOnlyApplies {
    if !mayWriteTo(
      readOnlyApply,
      sideEffects: loopSideEffects,
      aliasAnalysis: context.aliasAnalysis,
      calleeAnalysis: context.calleeAnalysis
    ) {
      movableInstructions.hoistUp.insert(readOnlyApply)
    }
  }
  
  // TODO: Check for size to avoid quadratic complexity
  for load in loads {
    if !mayWriteTo(load, sideEffects: loopSideEffects, aliasAnalysis: context.aliasAnalysis) {
      movableInstructions.hoistUp.insert(load)
    }
  }
  
  if !globalInitCalls.isEmpty {
    // TODO: Check for post dom tree root node. Check if post dominated side effects conflict with global initializer call.
  }
  
  if !hasOtherMemReadingInsts {
    for storeInst in stores {
      let accessPath = storeInst.destination.accessPath
      
      if accessPath.isLoopInvariant(loop: loop) {
        // TODO: Add remaining checks.
      }
    }
  }
  
  if !fixLifetimes.isEmpty {
    let sideEffectsMayRelease = loopSideEffects.contains(where: { $0.mayRelease })
    
    for fixLifetime in fixLifetimes {
      guard fixLifetime.operand.value.type.isAddress else { continue }
      
      if sideEffectsMayRelease || !mayWriteTo(fixLifetime, sideEffects: loopSideEffects, aliasAnalysis: context.aliasAnalysis) {
        movableInstructions.sinkDown.insert(fixLifetime)
      }
    }
  }
  
  for beginAccess in beginAccesses {
    guard handledEndAccess(beginAccessInst: beginAccess, loop: loop, context: context) else { continue }
    
    // TODO: Analyze begin access and add instructions to the special hoist.
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

private func checkSideEffects(
  inst: Instruction,
  loopSideEffects: inout Stack<Instruction>,
  blockSideEffects: inout Stack<Instruction>,
  hasOtherMemReadingInsts: inout Bool
) {
  if inst.mayHaveSideEffects {
    loopSideEffects.push(inst)
    blockSideEffects.push(inst)
  } else if inst.mayReadFromMemory {
    hasOtherMemReadingInsts = true
  }
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
  domTree: DominatorTree
) -> Bool {
  guard let preheader = loop.preheader else {
    return false
  }
  
  if inst is TermInst || inst is Allocation || inst is Deallocation {
    return false
  }
  
  // TODO: Handle properly the array semantics call.
  // FIXME: Hoist the entire cpp thing to avoid reimplementing a lot of stuff.
  
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

private extension AccessPath {
  func isLoopInvariant(loop: Loop) -> Bool {
    switch base {
    case .argument(let functionArgument):
       // TODO: Isn't function argument guaranteed to be an invariant?
      if loop.basicBlockSet.contains(functionArgument.parentBlock) {
        return false
      }
    case .box(let inst as Instruction), .class(let inst as Instruction), .index(let inst as Instruction),
         .pointer(let inst as Instruction), .stack(let inst as Instruction), .storeBorrow(let inst as Instruction),
         .tail(let inst as Instruction):
      if loop.basicBlockSet.contains(inst.parentBlock) {
        return false
      }
    case .global:
      break
    case .yield:
      break // TODO: What is yield?
    case .unidentified:
      return false
    }
    
    return !projectionPath.isConstant // TODO: Is this correct?
  }
}
