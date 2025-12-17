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

    // If the load_borrow is followed by a copy_value, combine both into a `load [copy]`:
    // ```
    //   %1 = load_borrow %0
    //   %2 = some_forwarding_instruction %1 // zero or more forwarding instructions
    //   %3 = copy_value %2
    //   end_borrow %1
    // ```
    // ->
    // ```
    //   %1 = load [copy] %0
    //   %3 = some_forwarding_instruction %1 // zero or more forwarding instructions
    // ```
    //
    tryCombineWithCopy(context)
  }

  private func tryCombineWithCopy(_ context: SimplifyContext) {
    let forwardedValue = lookThroughOwnedConvertibaleForwardingChain()
    guard let singleUser = forwardedValue.uses.ignore(usersOfType: EndBorrowInst.self).singleUse?.instruction,
          let copy = singleUser as? CopyValueInst,
          copy.parentBlock == self.parentBlock else {
      return
    }
    let builder = Builder(before: self, context)
    let loadCopy = builder.createLoad(fromAddress: address, ownership: .copy)
    let forwardedOwnedValue = replaceGuaranteed(value: self, withOwnedValue: loadCopy, context)
    copy.replace(with: forwardedOwnedValue, context)
    context.erase(instructionIncludingAllUsers: self)
  }
}
