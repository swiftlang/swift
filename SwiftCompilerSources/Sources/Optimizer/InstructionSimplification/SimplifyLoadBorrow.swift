//===--- SimplifyLoadBorrow.swift -----------------------------------------===//
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

extension LoadBorrowInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if uses.ignoreDebugUses.ignore(usersOfType: EndBorrowInst.self).isEmpty {
      context.erase(instructionIncludingAllUsers: self)
      return
    }

    if tryCombineWithCopy(context) {
      return
    }

    if tryRemoveAddrCast(context) {
      return
    }

    tryForwardStoreBorrow(context)
  }

  /// If the load_borrow is followed by a copy_value, combine both into a `load [copy]`:
  /// ```
  ///   %1 = load_borrow %0
  ///   %2 = some_forwarding_instruction %1 // zero or more forwarding instructions
  ///   %3 = copy_value %2
  ///   end_borrow %1
  /// ```
  /// ->
  /// ```
  ///   %1 = load [copy] %0
  ///   %3 = some_forwarding_instruction %1 // zero or more forwarding instructions
  /// ```
  ///
  private func tryCombineWithCopy(_ context: SimplifyContext) -> Bool {
    let forwardedValue = lookThroughOwnedConvertibaleForwardingChain()
    guard let singleUser = forwardedValue.uses.ignore(usersOfType: EndBorrowInst.self).singleUse?.instruction,
          let copy = singleUser as? CopyValueInst,
          copy.parentBlock == self.parentBlock else {
      return false
    }
    let builder = Builder(before: self, context)
    let loadCopy = builder.createLoad(fromAddress: address, ownership: .copy)
    let forwardedOwnedValue = replaceGuaranteed(value: self, withOwnedValue: loadCopy, context)
    copy.replace(with: forwardedOwnedValue, context)
    context.erase(instructionIncludingAllUsers: self)
    return true
  }

  /// Replaces address casts of heap objects
  /// ```
  ///   %1 = unchecked_addr_cast %0 : $*SomeClass to $*OtherClass
  ///   %2 = load_borrow %1
  ///   // ... uses of %2
  ///   end_borrow %2
  /// ```
  /// with ref-casts of the loaded value
  /// ```
  ///   %1 = load_borrow %0
  ///   %2 = unchecked_ref_cast %1 : $SomeClass to $OtherClass
  ///   // ... uses of %2
  ///   end_borrow %2
  /// ```
  /// Address casts are bad because they prevent alias analysis and AccessPath computation.
  /// It's always better to use the corresponding value casts instead.
  ///
  private func tryRemoveAddrCast(_ context: SimplifyContext) -> Bool {
    guard let addrCast = address.isAddressCastOfHeapObjects else {
      return false
    }
    let builder = Builder(before: self, context)
    let newLoad = builder.createLoadBorrow(fromAddress: addrCast.fromAddress)
    let cast = builder.createUncheckedRefCast(from: newLoad, to: addrCast.type.objectType)
    replace(with: newLoad, context)
    newLoad.uses.filter{ !$0.endsLifetime }.ignore(user: cast).replaceAll(with: cast, context)
    return true
  }

  /// Replaces a `load_borrow` of a `store_borrow` with a `begin_borrow`:
  /// ```
  ///   %1 = alloc_stack $T
  ///   %2 = store_borrow %0 to %1
  ///   ...
  ///   %3 = load_borrow %2
  ///   // ... uses of %3
  ///   end_borrow %3
  /// ```
  /// ->
  /// ```
  ///   %1 = alloc_stack $T
  ///   %2 = store_borrow %0 to %1
  ///   ...
  ///   %3 = begin_borrow %0
  ///   // ... uses of %3
  ///   end_borrow %3
  /// ```
  private func tryForwardStoreBorrow(_ context: SimplifyContext) {
    let accessPath = address.accessPath
    guard case .storeBorrow(let storeBorrow) = accessPath.base,
          accessPath.projectionPath.isMaterializable,
          uses.endingLifetime.ignore(usersOfType: EndBorrowInst.self).isEmpty
    else {
      return
    }

    let builder = Builder(before: self, context)
    let beginBorrow = builder.createBeginBorrow(of: storeBorrow.source)
    uses.endingLifetime.replaceAll(with: beginBorrow, context)
    let v = beginBorrow.createProjection(path: accessPath.projectionPath, builder: builder)
    replace(with: v, context)
  }
}
