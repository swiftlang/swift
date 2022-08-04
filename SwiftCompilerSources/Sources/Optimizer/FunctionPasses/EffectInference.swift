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
  var effectState = EffectState(function.entryBlock.arguments.endIndex, context.calleeAnalysis)

  for block in function.blocks {
    for inst in block.instructions {
      effectState.update(for: inst)
    }
  }

  context.modifyEffects(in: function, { effects in
    effects.globalEffects = effectState.globalEffects
    for (idx, (effect, path)) in effectState.argumentEffects.enumerated() {
      effects.argumentEffects.append(
        ArgumentEffect(
          .sideEffect(effect),
          selectedArg: ArgumentEffect.Selection(.argument(idx), pathPattern: effect.isPure ? SmallProjectionPath(): path!)
        )
      )
    }
  })
})


private struct EffectState : AccessStoragePathWalker {
  var argumentEffects: [(effect: SideEffect, path: SmallProjectionPath?)]
  var globalEffects: GlobalEffects = GlobalEffects(isValid: true)
  var localEffects: SideEffect = SideEffect()

  var accessPathWalker = AccessPathWalker()

  private let calleeAnalysis: CalleeAnalysis

  init(_ nargs: Int, _ calleeAnalysis: CalleeAnalysis) {
    argumentEffects = Array(repeating: (SideEffect(), nil), count: nargs)
    self.calleeAnalysis = calleeAnalysis
  }

  mutating
  func update(for inst: Instruction) {
    var matched = true
    switch inst {
    case let apply as FullApplySite:
      guard let callees = calleeAnalysis.getCallees(callee: apply.callee) else { return globalEffects.setWorstEffects() }

      for callee in callees {
        // Update global effects
        globalEffects.merge(with: callee.effects.globalEffects)

        // Update argument effects
        let arguments = apply.argumentOperands
        for effect in callee.effects.argumentEffects {
          if case let .sideEffect(sideEffect) = effect.kind {
            if let argIdx = apply.callerArgIndex(calleeArgIndex: effect.selectedArg.argumentIndex) {
              updateEffect(to: arguments[argIdx].value, { $0.merge(with: sideEffect) })
            } else {
              globalEffects.sideEffect.merge(with: sideEffect)
            }
          }
        }
      }
    case let fl as FixLifetimeInst:
      return updateEffect(to: fl.operand, { $0.read = true })
    case is AllocStackInst, is DeallocStackInst:
      break
    case let ba as BeginAccessInst:
      if !ba.isStatic {
        globalEffects.traps = true
      }
    case is EndAccessInst, is BeginBorrowInst, is EndBorrowInst:
      break
    case is AllocRefInst, is AllocRefDynamicInst:
      globalEffects.allocates = true
    case is StrongRetainInst, is RetainValueInst:
      // TODO: strong_retain_unowned etc?
      updateEffect(to: (inst as! UnaryInstruction).operand, {
        $0.retain = true
      })
    case is StrongReleaseInst, is ReleaseValueInst:
      updateEffect(to: (inst as! UnaryInstruction).operand, { $0.release = true })
    case let cast as UnconditionalCheckedCastInst:
      updateEffect(to: cast.operand, { $0.read = true })
      globalEffects.traps = true
    case let lb as LoadBorrowInst:
      updateEffect(to: lb.operand, { $0.read = true })
    case let load as LoadInst:
      updateEffect(to: load.operand, {
        $0.read = true
        // TODO: Ownership Qualifier?
      })
    case let store as StoreInst:
      updateEffect(to: store.destinationOperand.value, {
        $0.write = true
        if store.destinationOwnership == .assign {
          $0.release = true
        }
      })
    case is CondFailInst:
      globalEffects.traps = true
    case let pa as PartialApplyInst:
      for (i, arg) in pa.arguments.enumerated() {
        if pa.getArgumentConvention(calleeArgIndex: i).isIndirect {
          updateEffect(to: arg) { $0.read = true }
        }
      }
    case let bi as BuiltinInst:
      switch bi.id {
      case .IsUnique:
        globalEffects.readsRC = true
      case .CondUnreachable:
        globalEffects.traps = true
      default:
        matched = false
      }
    default:
      matched = false
    }

    if !matched {
      globalEffects.sideEffect.read = globalEffects.sideEffect.read || inst.mayReadFromMemory
      globalEffects.sideEffect.write = globalEffects.sideEffect.write || inst.mayWriteToMemory
      if inst.mayHaveSideEffects {
        globalEffects.setWorstEffects()
      }

      if inst.mayTrap {
        globalEffects.traps = true
      }
    }
  }

  private mutating
  func updateEffect(_ origin: Value, _ update: (inout SideEffect) -> (), _ path: SmallProjectionPath! = nil) {
    switch origin {
    case let arg as FunctionArgument:
      update(&argumentEffects[arg.index].effect)
      if let oldPath = argumentEffects[arg.index].path {
        argumentEffects[arg.index].path = oldPath.merge(with: path)
      } else {
        argumentEffects[arg.index].path = path
      }
    case is Allocation:
      update(&localEffects)
    default:
      update(&globalEffects.sideEffect)
    }
  }

  mutating func visit(access: AccessStoragePath) {
    updateEffect(access.storage, updateFn, access.path)
  }
  var updateFn: (inout SideEffect) -> () = {_ in }
  var walkUpCache = WalkerCache<Path>()

  private mutating
  func updateEffect(to value: Value, _ update: (inout SideEffect) -> ()) {
    if value.type.isAddress {
      guard let ap = accessPathWalker.getAccessPath(of: value) else { return update(&globalEffects.sideEffect) }
      let ba = ap.base.baseAddress
      if ba is Allocation {
        update(&localEffects)
      } else if let _ = ba as? FunctionArgument {
        updateEffect(ba, update, ap.projectionPath)
      } else if let _ = ap.base.reference {
        withoutActuallyEscaping(update) {
          updateFn = $0
          getAccessStorage(for: ap)
          updateFn = {_ in }
        }
      } else {
        update(&globalEffects.sideEffect)
      }
    } else {
      withoutActuallyEscaping(update) {
        updateFn = $0
        walkUpCache.clear()
        _ = walkUp(value: value, path: SmallProjectionPath())
        updateFn = {_ in }
      }
    }
  }
}
