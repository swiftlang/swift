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

struct SupportingDataStructures {
  let runsOnHighLevelSil: Bool
  let domTree: DominatorTree
  let postDomTree: PostDominatorTree
  let aliasAnalysis: AliasAnalysis
  let calleeAnalysis: CalleeAnalysis
}

struct DiscoveredMovableInstructions {
  let context: Context
  
  var loadsAndStores: InstructionSet
  var hoistUp: InstructionSet
  var sinkDown: InstructionSet
  var specialHoist: InstructionSet
  
  init(context: Context) {
    self.context = context
    
    self.loadsAndStores = InstructionSet(context)
    self.hoistUp = InstructionSet(context)
    self.sinkDown = InstructionSet(context)
    self.specialHoist = InstructionSet(context)
  }
  
  mutating func clear() {
    deinitialize()
    
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

let loopInvariantCodeMotionPass = FunctionPass(name: "loop-invariant-code-motion") { function, context in
  let loopTree = context.loopTree
  
  guard !loopTree.loops.isEmpty else { return }
  
  let dataStructures = SupportingDataStructures(
    runsOnHighLevelSil: true, // TODO: Make a parameter.
    domTree: context.dominatorTree,
    postDomTree: context.postDominatorTree,
    aliasAnalysis: context.aliasAnalysis,
    calleeAnalysis: context.calleeAnalysis
  )
  
  var changed = false
  
  for loop in loopTree.loops {
    changed = optimizeTopLevelLoop(
      topLevelLoop: loop,
      dataStructures: dataStructures,
      context: context
    ) || changed
  }
}

private func optimizeTopLevelLoop(
  topLevelLoop: Loop,
  dataStructures: SupportingDataStructures,
  context: Context
) -> Bool {
  var workList = [topLevelLoop]
  var i = 0
  while i < workList.count {
    let thisLoop = workList[i]
    workList.append(contentsOf: thisLoop.innerLoops)
    i += 1
  }
  
  var movableInstructions = DiscoveredMovableInstructions(context: context)
  
  var changed = false
  
  while !workList.isEmpty {
    let thisLoop = workList.popLast()!
    
    // TODO: Compute and propagate the summaries (Might not be necessary)
    
    var thisLoopChanged = false
    
    repeat {
      analyzeLoop(loop: thisLoop, dataStructures: dataStructures, movableInstructions: &movableInstructions, context: context)
      
      thisLoopChanged = optimizeLoop(loop: thisLoop, movableInstructions: movableInstructions)
      
      if thisLoopChanged {
        changed = true
      }
      
      movableInstructions.clear()
    } while thisLoopChanged
  }
  
  movableInstructions.deinitialize()
  
  return changed
}

private func analyzeLoop(
  loop: Loop,
  dataStructures: SupportingDataStructures,
  movableInstructions: inout DiscoveredMovableInstructions,
  context: Context
) {
  let preheader = loop.bridged.getPreheader()
  
  var readOnlyApplies = Stack<ApplyInst>(context)
  var globalInitCalls = Stack<Instruction>(context)
  
  var loads = Stack<LoadInst>(context)
  var stores = Stack<StoreInst>(context)
  var fixLifetimes = Stack<FixLifetimeInst>(context)
  var beginAccesses = Stack<BeginAccessInst>(context)
  var fullApplies = Stack<FullApplySite>(context)
  
  var hasOtherMemReadingInsts = false
  
  for bb in loop.basicBlocks {
    var blockSideEffects = InstructionSet(context)
    defer {
      blockSideEffects.deinitialize()
    }
    
    for inst in bb.instructions {
      if hasOwnershipOperandsOrResults(inst: inst) {
        checkSideEffects(
          inst: inst,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
        
        guard let fullApply = inst as? FullApplySite else { continue }
        
        fullApplies.push(fullApply)
      }
      
      switch inst {
      case let fixLifetimeInst as FixLifetimeInst:
        if dataStructures.domTree.bridged.dominates(fixLifetimeInst.operand.value.parentBlock.bridged, preheader) {
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
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let beginAccessInst as BeginAccessInst:
        beginAccesses.push(beginAccessInst)
        checkSideEffects(
          inst: beginAccessInst,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let refElementAddrInst as RefElementAddrInst:
        movableInstructions.specialHoist.insert(refElementAddrInst)
      case let condFailInst as CondFailInst:
        movableInstructions.hoistUp.insert(condFailInst)
        checkSideEffects(
          inst: condFailInst,
          blockSideEffects: &blockSideEffects,
          hasOtherMemReadingInsts: &hasOtherMemReadingInsts
        )
      case let applyInst as ApplyInst:
        // TODO: Handle apply inst
        fallthrough
      default:
        switch inst {
        case let fullApply as FullApplySite:
          fullApplies.push(fullApply)
        case let builtinInst as BuiltinInst:
          switch builtinInst.id {
          case .Once, .OnceWithContext:
            // TODO: Check if it collides with global init
          default: break;
          }
        default: break
        }
        
        checkSideEffects(inst: inst, blockSideEffects: &blockSideEffects, hasOtherMemReadingInsts: &hasOtherMemReadingInsts)
        
        if canHoistUpDefault(inst: inst, loop: loop, dataStructures: dataStructures) {
          movableInstructions.hoistUp.insert(inst)
        }
      }
    }
  }
  
  // TODO: Iterated through the stacks with extracted instructions.
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
  blockSideEffects: inout InstructionSet,
  hasOtherMemReadingInsts: inout Bool
) {
  if inst.mayHaveSideEffects {
    // TODO: Might have to propagate side effects to function wide side effects set.
    blockSideEffects.insert(inst)
  } else if inst.mayReadFromMemory {
    hasOtherMemReadingInsts = true
  }
}

private func hasOwnershipOperandsOrResults(inst: Instruction) -> Bool {
  guard inst.parentFunction.hasOwnership else { return false }
  
  // TODO: Double check whether .nonUse is equivalent to .none
  if inst.results.contains(where: { $0.ownership != .none }) ||
     inst.operands.contains(where: { $0.ownership != .nonUse }) {
    return true
  }
  
  return false
}

private func isSafeReadOnlyApply(
  applyInst: ApplyInst,
  dataStructures: SupportingDataStructures
) -> Bool {
  // TODO: Bridge and check applyInst.getSingleResult()
  
  if dataStructures.runsOnHighLevelSil {
    // TODO: Figure out how to bridge and check array semantics call.
  }
  
  // TODO: Bridge calleeAnalysis.getMemoryBehavior()
  return false
}

private func canHoistUpDefault(
  inst: Instruction,
  loop: Loop,
  dataStructures: SupportingDataStructures
) -> Bool {
  let preheader = loop.bridged.getPreheader()
  // TODO: Preheader could be null_ptr. Check how to bridge optional.
  
  if inst is TermInst || inst is Allocation || inst is Deallocation {
    return false
  }
  
  // TODO: Handle properly the array semantics call.
  
  // TODO: Is this equivalent to inst->getMemoryBehavior() == MemoryBehavior::None
  if inst.memoryEffects == .noEffects {
    return true
  }
  
  return false
}
