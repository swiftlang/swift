//===--- EffectInference.swift - Computes side-effects          ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

let effectInference = FunctionPass(name: "infer-effects", {
  (function: Function, context: PassContext) in
//  var effectState = EffectState(function.entryBlock.arguments.endIndex, context)


  var effectState2 = EffectState2(nargs: function.entryBlock.arguments.endIndex, calleeAnalysis: context.calleeAnalysis)

  print("Function: \(function.name)")
  for block in function.blocks {
    for inst in block.instructions {
      effectState2.updateWith(effectsOf: inst)
      print("argEffects ", effectState2.argumentEffects)
      print("globalEffects ", effectState2.globalEffects)
      print()
//      effectState.update(for: inst)
    }
  }

  context.modifyEffects(in: function) { effects in
    effects.globalEffects = effectState2.globalEffects
    for i in 0..<(effectState2.argumentEffects.count / effectState2.effectsPerArg) {
      for j in 0..<effectState2.effectsPerArg {
        if let effect = effectState2.argumentEffects[i * effectState2.effectsPerArg + j] {
          switch effect {
          case .worst: break
          case let .only(path, effect):
            switch effect {
            case let .memory(effect):
              effects.argumentEffects.append(
                ArgumentEffect(
                  .memory(effect),
                  selectedArg: ArgumentEffect.Selection(.argument(i), pathPattern: path)
                )
              )
              continue
            case let .ownership(effect):
              effects.argumentEffects.append(
                ArgumentEffect(
                  .ownership(effect),
                  selectedArg: ArgumentEffect.Selection(.argument(i), pathPattern: path)
                )
              )
              continue
            default:
              fatalError("unreacheable")
            }
          }
        }
        break
      }
    }
  }
  print("\n\n")

  // context.modifyEffects(in: function, { effects in
  //   effects.globalEffects = effectState.globalEffects
  //   for (idx, (effect, path)) in effectState.argumentEffects.enumerated() {
  //     effects.argumentEffects.append(
  //       ArgumentEffect(
  //         .sideEffect(effect),
  //         selectedArg: ArgumentEffect.Selection(.argument(idx), pathPattern: effect.isPure ? SmallProjectionPath(): path!)
  //       )
  //     )
  //   }
  // })
})


private struct ReferenceRoots : ValueUseDefWalker {
  typealias Path = SmallProjectionPath
  
  init(context: PassContext) {
    results = Stack(context)
  }

  mutating func find(for reference: Value) -> Stack<(Value, Path)> {
    _ = walkUp(value: reference, path: Path())
    return results
  }

  mutating func rootDef(value: Value, path: Path) -> WalkResult {
    results.push((value, path))
    return .continueWalk
  }

  var walkUpCache = WalkerCache<Path>()
  private(set) var results: Stack<(Value, Path)>
}

struct Effect {
  let memory: MemoryEffect
  let ownership: OwnershipEffect
  init(_ memory: MemoryEffect, _ ownership: OwnershipEffect) {
    self.memory = memory
    self.ownership = ownership
  }

  func merge(with other: Effect) -> Effect {
    Effect(
      memory.merge(with: other.memory),
      ownership.merge(with: other.ownership)
    )
  }
}

private struct EffectState2 : AccessStoragePathWalker {
  init(nargs: Int, calleeAnalysis: CalleeAnalysis) {
    self.argumentEffects = Array(repeating: nil, count: nargs * effectsPerArg)
    self.calleeAnalysis = calleeAnalysis
  }

  var calleeAnalysis: CalleeAnalysis

  // argument i has memory effect at i * k + j with 0 <= j < k
  var effectsPerArg = 2
  
  enum ArgEffect {
    case worst
    case only(SmallProjectionPath, ArgumentEffect.Kind)
    
    func mergeIfSamePath(path: SmallProjectionPath, effect: ArgumentEffect.Kind) -> ArgEffect? {
      switch self {
      case .worst:
        return .worst
      case let .only(selfPath, selfEffect) where selfPath.matches(pattern: path):
        switch (selfEffect, effect) {
        case let (.memory(selfMemEffect), .memory(memEffect)):
          return .only(selfPath, .memory(selfMemEffect.merge(with: memEffect)))
        case let (.ownership(selfOwnEffect), .ownership(ownEffect)):
          return .only(selfPath, .ownership(selfOwnEffect.merge(with: ownEffect)))
        default:
          return nil
        }
      default:
        return nil
      }
    }
  }

  var argumentEffects: [ArgEffect?]
  var globalEffects: GlobalEffects = GlobalEffects(isValid: true)

  var unknownEffects: Set<Operand> = Set()
  var accessPathWalker = AccessPathWalker()

  var currOperand: Operand! = nil
  var currEffect: ArgumentEffect.Kind = .memory(MemoryEffect(read: true, write: false))
  
  // Records the `effect` in the argument effects
  mutating func updateArgumentEffect(arg: FunctionArgument, path: SmallProjectionPath, effect: ArgumentEffect.Kind) {
    let start = arg.index * effectsPerArg
    
    for i in 0..<effectsPerArg {
      if let oldEffect = argumentEffects[start + i] {
        // If there is a worst effect
        if let mergedEffect = oldEffect.mergeIfSamePath(path: path, effect: effect) {
          argumentEffects[start + i] = mergedEffect
          return
        }
      } else {
        argumentEffects[start + i] = .only(path, effect)
        return
      }
    }
    
    argumentEffects[start] = .worst
  }

  mutating func visit(access: AccessStoragePath) {
    switch access.storage {
    case let arg as FunctionArgument:
      updateArgumentEffect(arg: arg, path: access.path, effect: currEffect)
    case is AllocRefInst, is AllocRefDynamicInst:
      break
    default:
      unknownEffects.insert(currOperand)
    }
  }

  mutating func updateAccessStorages(on operand: Operand, for ap: AccessPath, with effect: ArgumentEffect.Kind) {
    let tmpEffect = currEffect, tmpOperand = currOperand
    currEffect = effect
    currOperand = operand
    defer {
      currEffect = tmpEffect
      currOperand = tmpOperand
    }
    getAccessStorage(for: ap)
  }

  mutating func updateAccessStorages(for reference: Operand, with effect: ArgumentEffect.Kind) {
    let tmpEffect = currEffect, tmpOperand = currOperand
    currEffect = effect
    currOperand = reference
    defer {
      currEffect = tmpEffect
      currOperand = tmpOperand
    }
    walkUpCache.clear()
    _ = walkUp(value: reference.value, path: SmallProjectionPath())
  }

  mutating func updateOriginEffects(for operand: Operand, effect: ArgumentEffect.Kind) {
    if operand.value.type.isAddress {
      guard let ap = accessPathWalker.getAccessPath(of: operand.value) else {
        unknownEffects.insert(operand)
        return
      }
      let ba = ap.base.baseAddress
      if ba is Allocation {
        // Do nothing
      } else if let arg = ba as? FunctionArgument {
        updateArgumentEffect(arg: arg, path: ap.projectionPath, effect: effect)
      } else if let _ = ap.base.reference {
        updateAccessStorages(on: operand, for: ap, with: effect)
      } else {
        unknownEffects.insert(operand)
      }
    } else {
      updateAccessStorages(for: operand, with: effect)
    }
  }

  mutating func updateWith(effectsOf instruction: Instruction) {
    var updates: [(Operand, ArgumentEffect.Kind)] = []

    var matched = true
    switch instruction {
    case is AllocStackInst, is DeallocStackInst, is EndAccessInst,
        is BeginBorrowInst, is EndBorrowInst:
      break
    case is AllocRefInst, is AllocRefDynamicInst:
      globalEffects.allocates = true
    case let ba as BeginAccessInst where !ba.isStatic:
      globalEffects.traps = true
    case is CondFailInst:
      globalEffects.traps = true
    case let bi as BuiltinInst:
      switch bi.id {
      case .IsUnique:
        globalEffects.readsRC = true
      case .CondUnreachable:
        globalEffects.traps = true
      default:
        matched = false
      }
    case is LoadInst, is LoadWeakInst, is LoadUnownedInst, is LoadBorrowInst:
      let load = instruction as! UnaryInstruction
      updates.append((load.operands[0], .memory(MemoryEffect(read: true, write: false))))
    case is StoreInst, is StoreWeakInst, is StoreUnownedInst:
      let store = instruction as! StoringInstruction
      // if the destination operand is a reference, then the destination
      // is loaded and destroyed
      if let si = store as? StoreInst, si.destinationOwnership == .assign {
        updates.append((store.destinationOperand, .memory(MemoryEffect(read: true, write: true))))
        updates.append((store.destinationOperand, .ownership(OwnershipEffect(copy: false, destroy: true))))
      } else {
        updates.append((store.destinationOperand, .memory(MemoryEffect(read: false, write: true))))
      }
    case is CopyValueInst, is StrongRetainInst:
      let target = instruction as! UnaryInstruction
      updates.append((target.operands[0], .ownership(OwnershipEffect(copy: true, destroy: false))))
    case is DestroyValueInst, is StrongReleaseInst:
      let target = instruction as! UnaryInstruction
      updates.append((target.operands[0], .ownership(OwnershipEffect(copy: false, destroy: true))))
    case let apply as FullApplySite:
      guard let callees = calleeAnalysis.getCallees(callee: apply.callee) else {
        globalEffects.setWorstEffects()
        for arg in apply.operands {
          unknownEffects.insert(arg)
        }
        return
      }

      for callee in callees {
        // FIXME: globalEffects removed (?)
        globalEffects.merge(with: callee.effects.globalEffects)

        // Update argument effects
        let arguments = apply.argumentOperands
        for effect in callee.effects.argumentEffects {
          if case let .sideEffect(sideEffect) = effect.kind {
            if let argIdx = apply.callerArgIndex(calleeArgIndex: effect.selectedArg.argumentIndex) {
              fatalError("TODO: add caleesideeffect")
              // updates.append((arguments[argIdx].value, sideEffect))
            } else {
              fatalError("What to do in this case?")
            }
          }
        }
      }
    default:
      matched = false
    }

    if !matched {
      // TODO: update global effects
      for operand in instruction.operands {
        unknownEffects.insert(operand)
      }
      globalEffects.sideEffect.read = globalEffects.sideEffect.read || instruction.mayReadFromMemory
      globalEffects.sideEffect.write = globalEffects.sideEffect.write || instruction.mayWriteToMemory
      if instruction.mayHaveSideEffects {
        globalEffects.setWorstEffects()
      }

      if instruction.mayTrap {
        globalEffects.traps = true
      }
    }

    for (operand, effect) in updates {
      updateOriginEffects(for: operand, effect: effect)
    }

    // TODO: check argument convention to know whether adding
    //   the effect is meaningful
    // TODO: add def-use walk to check whether all uses are known
    //   and global effects don't interfere with them
  }

  // `AccessStoragePathWalker` conformance
  var walkUpCache = WalkerCache<SmallProjectionPath>()
}

struct CheckUses : AccessUseVisitor {
  let unknownUses: Set<Operand>
  
  mutating func visitUse(value: Operand, path: Path) -> WalkResult {
    return unknownUses.contains(value) ? .abortWalk : .continueWalk
  }
  
  mutating func visitUse(address: Operand, path: Path) -> WalkResult {
    return unknownUses.contains(address) ? .abortWalk : .continueWalk
  }
  
  var walkDownCache = WalkerCache<SmallProjectionPath>()
}
//
//private struct EffectState : AccessStoragePathWalker {
//  var argumentEffects: [(effect: SideEffect, path: SmallProjectionPath?)]
//  var globalEffects: GlobalEffects = GlobalEffects(isValid: true)
//  var localEffects: SideEffect = SideEffect()
//
//  var accessPathWalker = AccessPathWalker()
//
//  private var calleeAnalysis: CalleeAnalysis {
//    context.calleeAnalysis
//  }
//  private var context: PassContext
//
//  init(_ nargs: Int, _ context: PassContext) {
//    argumentEffects = Array(repeating: (SideEffect(), nil), count: nargs)
//    self.context = context
//  }
//
//  mutating
//  func update(for inst: Instruction) {
//    var matched = true
//    switch inst {
//    case let apply as FullApplySite:
//      guard let callees = calleeAnalysis.getCallees(callee: apply.callee) else { return globalEffects.setWorstEffects() }
//
//      for callee in callees {
//        // Update global effects
//        globalEffects.merge(with: callee.effects.globalEffects)
//
//        // Update argument effects
//        let arguments = apply.argumentOperands
//        for effect in callee.effects.argumentEffects {
//          if case let .sideEffect(sideEffect) = effect.kind {
//            if let argIdx = apply.callerArgIndex(calleeArgIndex: effect.selectedArg.argumentIndex) {
//              updateEffect(to: arguments[argIdx].value, { $0.merge(with: sideEffect) })
//            } else {
//              globalEffects.sideEffect.merge(with: sideEffect)
//            }
//          }
//        }
//      }
//    case let fl as FixLifetimeInst:
//      // A fix_lifetime instruction acts like a read on the operand. Retains can
//      // move after it but the last release can't move before it.
//      return updateEffect(to: fl.operand, { $0.read = true })
//    case is AllocStackInst, is DeallocStackInst:
//      break
//    case let ba as BeginAccessInst:
//      if !ba.isStatic {
//        globalEffects.traps = true
//      }
//    case is EndAccessInst, is BeginBorrowInst, is EndBorrowInst:
//      break
//    case is AllocRefInst, is AllocRefDynamicInst:
//      globalEffects.allocates = true
//    case is StrongRetainInst, is RetainValueInst:
//      // TODO: strong_retain_unowned etc?
//      updateEffect(to: (inst as! UnaryInstruction).operand, {
//        $0.retain = true
//      })
//    case is StrongReleaseInst, is ReleaseValueInst:
//      updateEffect(to: (inst as! UnaryInstruction).operand, { $0.release = true })
//    case let cast as UnconditionalCheckedCastInst:
//      updateEffect(to: cast.operand, { $0.read = true })
//      globalEffects.traps = true
//    case let lb as LoadBorrowInst:
//      updateEffect(to: lb.operand, { $0.read = true })
//    case let load as LoadInst:
//      updateEffect(to: load.operand, {
//        $0.read = true
//        // TODO: Ownership Qualifier?
//      })
//    case let store as StoreInst:
//      updateEffect(to: store.destinationOperand.value, {
//        $0.write = true
//        if store.destinationOwnership == .assign {
//          $0.release = true
//        }
//      })
//    case is CondFailInst:
//      globalEffects.traps = true
//    case let pa as PartialApplyInst:
//      for (i, arg) in pa.arguments.enumerated() {
//        if pa.getArgumentConvention(calleeArgIndex: i).isIndirect {
//          updateEffect(to: arg) { $0.read = true }
//        }
//      }
//    case let bi as BuiltinInst:
//      switch bi.id {
//      case .IsUnique:
//        globalEffects.readsRC = true
//      case .CondUnreachable:
//        globalEffects.traps = true
//      default:
//        matched = false
//      }
//    default:
//      matched = false
//    }
//
//    if !matched {
//      globalEffects.sideEffect.read = globalEffects.sideEffect.read || inst.mayReadFromMemory
//      globalEffects.sideEffect.write = globalEffects.sideEffect.write || inst.mayWriteToMemory
//      if inst.mayHaveSideEffects {
//        globalEffects.setWorstEffects()
//      }
//
//      if inst.mayTrap {
//        globalEffects.traps = true
//      }
//    }
//  }
//
//  private mutating
//  func updateEffect(_ origin: Value, _ update: (inout SideEffect) -> (), _ path: SmallProjectionPath! = nil) {
//    switch origin {
//    case let arg as FunctionArgument:
//      update(&argumentEffects[arg.index].effect)
//      if let oldPath = argumentEffects[arg.index].path {
//        argumentEffects[arg.index].path = oldPath.merge(with: path)
//      } else {
//        argumentEffects[arg.index].path = path
//      }
//    case is Allocation:
//      update(&localEffects)
//    default:
//      update(&globalEffects.sideEffect)
//    }
//  }
//
//  mutating func visit(access: AccessStoragePath) {
//    updateEffect(access.storage, updateFn, access.path)
//  }
//  var updateFn: (inout SideEffect) -> () = {_ in }
//  var walkUpCache = WalkerCache<Path>()
//
//  private mutating
//  func updateEffect(to value: Value, _ update: (inout SideEffect) -> ()) {
//    if value.type.isAddress {
//      guard let ap = accessPathWalker.getAccessPath(of: value) else { return update(&globalEffects.sideEffect) }
//      let ba = ap.base.baseAddress
//      if ba is Allocation {
//        update(&localEffects)
//      } else if let _ = ba as? FunctionArgument {
//        updateEffect(ba, update, ap.projectionPath)
//      } else if let _ = ap.base.reference {
//        withoutActuallyEscaping(update) {
//          updateFn = $0
//          getAccessStorage(for: ap)
//          updateFn = {_ in }
//        }
//      } else {
//        update(&globalEffects.sideEffect)
//      }
//    } else {
//      withoutActuallyEscaping(update) {
//        updateFn = $0
//        walkUpCache.clear()
//        _ = walkUp(value: value, path: SmallProjectionPath())
//        updateFn = {_ in }
//      }
//    }
//  }
//}
//

protocol AccessUseVisitor : ValueDefUseWalker, AddressDefUseWalker where Path == SmallProjectionPath {
  mutating func visitUse(value: Operand, path: Path) -> WalkResult
  mutating func visitUse(address: Operand, path: Path) -> WalkResult
}

extension AccessUseVisitor {
  mutating func visitAccesses(toAddress address: Value) -> WalkResult {
    assert(address.type.isAddress, "Expected address type")
    return walkDownUses(ofAddress: address, path: SmallProjectionPath(.anyValueFields))
  }

  mutating func visitUses(ofValue value: Value) -> WalkResult {
    // v**.c*.v**
    let path = SmallProjectionPath(.anyValueFields).push(.anyClassField).push(.anyValueFields)
    return walkDownUses(ofValue: value, path: path)
  }

  mutating func leafUse(value: Operand, path: Path) -> WalkResult {
    switch value.instruction {
    case let rta as RefTailAddrInst:
      if let path = path.popIfMatches(.tailElements, index: nil) {
        return walkDownUses(ofAddress: rta, path: path)
      }
    case let rea as RefElementAddrInst:
      if let path = path.popIfMatches(.classField, index: rea.fieldIndex) {
        return walkDownUses(ofAddress: rea, path: path)
      }
    case let pb as ProjectBoxInst:
      if let path = path.popIfMatches(.classField, index: pb.fieldIndex) {
        return walkDownUses(ofAddress: pb, path: path)
      }
    default:
      return visitUse(value: value, path: path)
    }
    fatalError("Unreachable")
  }

  mutating func leafUse(address: Operand, path: Path) -> WalkResult {
    return visitUse(address: address, path: path)
  }
}
