//===--- SimplifyOpenExistentialRef.swift ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension OpenExistentialRefInst: OnoneSimplifiable, SILCombineSimplifiable {

  /// Removes an existential pack/unpack pair when every use of the opened value is
  /// something we can redirect to the original (pre-existential) instance:
  ///
  ///   %1 = init_existential_ref %0 : $Klass : $Klass, $AnyObject
  ///   %2 = open_existential_ref %1 to $@opened(...) Self
  ///   %3 = begin_borrow %2
  ///   %4 = unchecked_ref_cast %3 to $Klass   // inside a borrow scope ...
  ///   end_borrow %3
  ///   %5 = unchecked_ref_cast %2 to $Klass   // ... or directly
  ///
  /// ->
  ///
  ///   %3 = begin_borrow %0
  ///   // replace %3 with %3
  ///   end_borrow %3
  ///   // replace %5 with %0
  ///
  /// For non-owned (guaranteed) existentials, IER/OER pairs can always be removed since forwarding
  /// a guaranteed value imposes no single-use restriction.
  /// For owned existentials, these instructions can only be removed if EVERY use of the opened
  /// existential is one of the two recognized shapes above.
  func simplify(_ context: SimplifyContext) {
    guard let ier = operand.value as? InitExistentialRefInst else {
      return
    }

    // In OSSA, redirecting an owned existential's uses is only safe if we can
    // account for (and later erase) every use of `self` -- otherwise we'd leave
    // two independent consumers of `ier.instance`. Guaranteed-OSSA and non-OSSA
    // code have no such consuming-use accounting, so partial redirection is
    // always safe there.
    let isOwned =
      ier.forwardingOwnership == .owned
      && ier.ownership == .owned
    if isOwned {
      guard ier.uses.ignoreDebugUses.isSingleUse else {
        return
      }
    }

    guard let (casts, beginBorrows) = collectRedirectableUses(bailOnUnrecognized: isOwned) else {
      return
    }

    for urc in casts {
      urc.operand.set(to: ier.instance, context)
    }
    for bb in beginBorrows {
      redirectBeginBorrow(bb, to: ier.instance, context)
    }

    // This will always be true for owned existentials.
    if uses.ignoreDebugUses.isEmpty {
      context.erase(instruction: self)
    } else {
      assert(!isOwned, "owned open_existental_ref must not have other uses")
    }

    if ier.uses.ignoreDebugUses.isEmpty {
      context.erase(instruction: ier)
    }
  }

  /// Classifies each non-debug use of `self` as either a direct `unchecked_ref_cast`
  /// to a concrete class-like type, or a `begin_borrow` scope whose own uses
  /// (besides `end_borrow`) are all such casts.
  ///
  /// If `bailOnUnrecognized` is true, any other use causes the whole classification to
  /// fail (returns nil); otherwise unrecognized uses are simply left untouched.
  private func collectRedirectableUses(bailOnUnrecognized: Bool)
    -> (casts: [UncheckedRefCastInst], beginBorrows: [BeginBorrowInst])?
  {
    var casts: [UncheckedRefCastInst] = []
    var beginBorrows: [BeginBorrowInst] = []
    for use in uses.ignoreDebugUses {
      if let bb = use.instruction as? BeginBorrowInst {
        guard isRedirectableBorrowScope(bb) else {
          if bailOnUnrecognized {
            return nil
          }
          continue
        }
        beginBorrows.append(bb)
      } else if let urc = use.instruction as? UncheckedRefCastInst,
        use == urc.operand,
        urc.type.isHeapObjectReferenceType
      {
        casts.append(urc)
      } else if bailOnUnrecognized {
        return nil
      }
    }
    return (casts, beginBorrows)
  }

  /// True if every non-debug, non-type-dependent use of `bb` other than its `end_borrow`s
  /// is an `unchecked_ref_cast` to a concrete class-like type.
  private func isRedirectableBorrowScope(_ bb: BeginBorrowInst) -> Bool {
    for bbUse in bb.uses.ignoreDebugUses {
      if bbUse.instruction is EndBorrowInst {
        continue
      }
      guard let urc = bbUse.instruction as? UncheckedRefCastInst, urc.type.isHeapObjectReferenceType
      else {
        return false
      }
    }
    return true
  }

  /// Rebuilds `bb`'s borrow scope over `newOperand` instead of its original operand, since a
  /// `begin_borrow`'s result type is fixed to its operand's type at construction.
  private func redirectBeginBorrow(
    _ bb: BeginBorrowInst, to newOperand: Value, _ context: SimplifyContext
  ) {
    let builder = Builder(before: bb, context)
    let newBeginBorrow = builder.createBeginBorrow(
      of: newOperand, isLexical: bb.isLexical, hasPointerEscape: bb.hasPointerEscape,
      isFromVarDecl: bb.isFromVarDecl)
    bb.uses.replaceAll(with: newBeginBorrow, context)
    context.erase(instruction: bb)
  }
}
