//===--- LetPropertyLowering.swift -----------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Lowers let property accesses of classes.
///
/// Lowering consists of two tasks:
///
/// * In class initializers, insert `end_init_let_ref` instructions at places where all let-fields are initialized.
///   This strictly separates the life-range of the class into a region where let fields are still written during
///   initialization and a region where let fields are truly immutable.
///
/// * Add the `[immutable]` flag to all `ref_element_addr` instructions (for let-fields) which are in the "immutable"
///   region. This includes the region after an inserted `end_init_let_ref` in an class initializer, but also all
///   let-field accesses in other functions than the initializer and the destructor.
///
/// This pass should run after DefiniteInitialization but before RawSILInstLowering (because it relies on
/// `mark_uninitialized` still present in the class initializer).
///
/// Note that it's not mandatory to run this pass. If it doesn't run, SIL is still correct.
///
/// Simplified example (after lowering):
///
///   bb0(%0 : @owned C):                           // = self of the class initializer
///     %1 = mark_uninitialized %0
///     %2 = ref_element_addr %1, #C.l              // a let-field
///     store %init_value to %2
///     %3 = end_init_let_ref %1                    // inserted by lowering
///     %4 = ref_element_addr [immutable] %3, #C.l  // set to immutable by lowering
///     %5 = load %4
///
let letPropertyLowering = FunctionPass(name: "let-property-lowering") {
  (function: Function, context: FunctionPassContext) in

  assert(context.silStage == .raw, "let-property-lowering must run before RawSILInstLowering")

  if context.hadError {
    // If DefiniteInitialization (or other passes) already reported an error, we cannot assume valid SIL anymore.
    return
  }

  if function.isDestructor {
    // Let-fields are not immutable in the class destructor.
    return
  }

  for inst in function.instructions {
    switch inst {

    // First task of lowering: insert `end_init_let_ref` instructions in class initializers.
    case let markUninitialized as MarkUninitializedInst
         where markUninitialized.type.isClass &&
               // TODO: support move-only classes
               !markUninitialized.type.isMoveOnly &&
               // We only have to do that for root classes because derived classes call the super-initializer
               // _after_ all fields in the derived class are already initialized.
               markUninitialized.kind == .rootSelf:

      insertEndInitInstructions(for: markUninitialized, context)

    // Second task of lowering: set the `immutable` flags.
    case let rea as RefElementAddrInst
         where rea.fieldIsLet && !rea.isInUninitializedRegion &&
               // TODO: support move-only classes
               !rea.instance.type.isMoveOnly:
      rea.set(isImmutable: true, context)

    default:
      break
    }
  }
}

private func insertEndInitInstructions(for markUninitialized: MarkUninitializedInst, _ context: FunctionPassContext) {
  assert(!markUninitialized.type.isAddress, "self of class should not be an address")

  // The region which contains all let-field initializations, including any partial
  // let-field de-initializations (in case of a fail-able or throwing initializer).
  var initRegion = InstructionRange(begin: markUninitialized, context)
  defer { initRegion.deinitialize() }

  constructLetInitRegion(of: markUninitialized, result: &initRegion, context)

  insertEndInitInstructions(for: markUninitialized, atEndOf: initRegion, context)
}

private func insertEndInitInstructions(
  for markUninitialized: MarkUninitializedInst,
  atEndOf initRegion: InstructionRange,
  _ context: FunctionPassContext
) {
  var ssaUpdater = SSAUpdater(function: markUninitialized.parentFunction,
                              type: markUninitialized.type, ownership: .owned, context)
  ssaUpdater.addAvailableValue(markUninitialized, in: markUninitialized.parentBlock)

  for endInst in initRegion.ends {
    let builder = Builder(after: endInst, context)
    let newValue = builder.createEndInitLetRef(operand: markUninitialized)
    ssaUpdater.addAvailableValue(newValue, in: endInst.parentBlock)
  }

  for exitInst in initRegion.exits {
    let builder = Builder(before: exitInst, context)
    let newValue = builder.createEndInitLetRef(operand: markUninitialized)
    ssaUpdater.addAvailableValue(newValue, in: exitInst.parentBlock)
  }

  for use in markUninitialized.uses {
    if !initRegion.inclusiveRangeContains(use.instruction) &&
       !(use.instruction is EndInitLetRefInst)
    {
      use.set(to: ssaUpdater.getValue(atEndOf: use.instruction.parentBlock), context)
    }
  }
}

private func constructLetInitRegion(
  of markUninitialized: MarkUninitializedInst,
  result initRegion: inout InstructionRange,
  _ context: FunctionPassContext
) {
  // Adding the initial `mark_uninitialized` ensures that a single `end_init_let_ref` is inserted (after the
  // `mark_uninitialized`) in case there are no let-field accesses at all.
  // Note that we have to insert an `end_init_let_ref` even if there are no let-field initializations, because
  // derived classes could have let-field initializations in their initializers (which eventually call the
  // root-class initializer).
  initRegion.insert(markUninitialized)

  var borrows = Stack<BeginBorrowInstruction>(context)
  defer { borrows.deinitialize() }

  for inst in markUninitialized.parentFunction.instructions {
    switch inst {
    case let assign as AssignInst
         where assign.destination.isLetFieldAddress(of: markUninitialized):
      assert(assign.assignOwnership == .initialize)
      initRegion.insert(inst)

    case let store as StoreInst
         where store.destination.isLetFieldAddress(of: markUninitialized):
      assert(store.storeOwnership != .assign)
      initRegion.insert(inst)

    case let copy as CopyAddrInst
         where copy.destination.isLetFieldAddress(of: markUninitialized):
      assert(copy.isInitializationOfDestination)
      initRegion.insert(inst)

    case let beginAccess as BeginAccessInst
         where beginAccess.accessKind == .deinit &&
               beginAccess.address.isLetFieldAddress(of: markUninitialized):
      // Include let-field partial de-initializations in the region.
      initRegion.insert(inst)

    case let beginBorrow as BeginBorrowInst
           where beginBorrow.borrowedValue.isReferenceDerived(from: markUninitialized):
      borrows.append(beginBorrow)

    case let storeBorrow as StoreBorrowInst
           where storeBorrow.source.isReferenceDerived(from: markUninitialized):
      borrows.append(storeBorrow)

    default:
      break
    }
  }

  // Extend the region to whole borrow scopes to avoid that we insert an `end_init_let_ref` in the
  // middle of a borrow scope.
  for borrow in borrows where initRegion.contains(borrow) {
    initRegion.insert(borrowScopeOf: borrow, context)
  }
}

private extension RefElementAddrInst {
  var isInUninitializedRegion: Bool {
    var root = self.instance
    while true {
      switch root {
      case let beginBorrow as BeginBorrowInst:
        root = beginBorrow.borrowedValue
      case let loadBorrow as LoadBorrowInst:
        // Initializers of derived classes store `self` into a stack location from where
        // it's loaded via a `load_borrow`.
        root = loadBorrow.address
      case is MarkUninitializedInst:
        return true
      default:
        return false
      }
    }
  }
}

private extension Value {
  func isReferenceDerived(from root: Value) -> Bool {
    var parent: Value = self
    while true {
      if parent == root {
        return true
      }
      if let operand = parent.forwardingInstruction?.singleForwardedOperand {
        parent = operand.value
        continue
      }
      if let transition = parent.definingInstruction as? OwnershipTransitionInstruction {
        parent = transition.operand.value
        continue
      }
      return false
    }
  }

  func isLetFieldAddress(of markUninitialized: MarkUninitializedInst) -> Bool {
    if case .class(let rea) = self.accessBase,
       rea.fieldIsLet,
       rea.instance.isReferenceDerived(from: markUninitialized)
    {
      return true
    }
    return false
  }
}
