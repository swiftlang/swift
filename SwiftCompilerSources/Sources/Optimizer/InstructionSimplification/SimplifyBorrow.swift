//===--- SimplifyBorrow.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension InitBorrowAddrInst: OnoneSimplifiable, SILCombineSimplifiable {
  // Rewrite uses of non-generic 'init_borrow_addr' into concrete
  // 'make_[addr_]borrow' stores.
  func simplify(_ context: SimplifyContext) {
    // If our borrow type is known statically to be loadable at this point, then
    // we can rewrite uses of 'init_borrow_addr' into respective
    // 'make_[addr_]borrow'.
    guard borrow.type.isLoadable(in: parentFunction) else {
      return
    }

    var loadable = false

    // The referent is only loadable if 1. it says its loadable and 2. it is NOT
    // @_addressableFroDependencies.
    if referent.type.isLoadable(in: parentFunction),
        !referent.type.isAddressableForDeps(in: parentFunction) {
      loadable = true
    }

    let builder = Builder(before: self, context)

    let newBorrow: Value

    // If our referent is loadable, then rewrite:
    //
    //   init_borrow_addr %borrow_addr with %referent_addr
    // -->
    //   %referent = load_borrow %referent_addr
    //   %borrow = make_borrow %referent
    //   store %borrow to %borrow_addr
    //
    // Otherwise, our referent is address only for borrows, so rewrite uses into
    // the address form:
    //
    //   init_borrow_addr %borrow_addr with %referent_addr
    // -->
    //   %borrow = make_addr_borrow %referent_addr
    //   store %borrow to %borrow_addr
    //
    if loadable {
      let loadedReferent = builder.emitLoadBorrow(fromAddress: referent)
      newBorrow = builder.createMakeBorrow(referent: loadedReferent)
    } else {
      newBorrow = builder.createMakeAddrBorrow(referent: referent)
    }

    let storeOwnership = StoreInst.StoreOwnership(
      for: borrow.type,
      in: parentFunction,
      initialize: true
    )

    builder.createStore(
      source: newBorrow,
      destination: borrow,
      ownership: storeOwnership
    )

    context.erase(instruction: self)
  }
}

extension DereferenceBorrowInst: OnoneSimplifiable, SILCombineSimplifiable {
  /// ```
  ///   %borrow = make_borrow %referent
  ///   %deref = dereference_borrow %borrow
  /// ```
  /// is transformed to
  /// ```
  ///   %referent
  /// ```
  func simplify(_ context: SimplifyContext) {
    guard let makeBorrow = borrow as? MakeBorrowInst else {
      return
    }

    replace(with: makeBorrow.referent, context)
  }
}

extension DereferenceAddrBorrowInst: OnoneSimplifiable, SILCombineSimplifiable {
  /// ```
  ///   %borrow = make_addr_borrow %referent
  ///   %deref = dereference_addr_borrow %borrow
  /// ```
  /// is transformed to
  /// ```
  ///   %referent
  /// ```
  func simplify(_ context: SimplifyContext) {
    guard let makeAddrBorrow = borrow as? MakeAddrBorrowInst else {
      return
    }

    replace(with: makeAddrBorrow.referent, context)
  }
}
