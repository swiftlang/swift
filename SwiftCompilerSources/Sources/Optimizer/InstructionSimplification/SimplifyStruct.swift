//===--- SimplifyStruct.swift ---------------------------------------------===//
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

extension StructInst : Simplifiable, SILCombineSimplifiable {

  /// Eliminates `struct_extract`s of an owned `struct` where the `struct_extract`s are inside a
  /// a borrow scope.
  /// This is done by splitting the `begin_borrow` of the whole struct into individual borrows of the fields
  /// (for trivial fields no borrow is needed). And then sinking the `struct` to it's consuming use(s).
  ///
  /// ```
  ///   %3 = struct $S(%nonTrivialField, %trivialField)  // owned
  ///   ...
  ///   %4 = begin_borrow %3
  ///   %5 = struct_extract %4, #S.nonTrivialField
  ///   %6 = struct_extract %4, #S.trivialField
  ///   use %5, %6
  ///   end_borrow %4
  ///   ...
  ///   end_of_lifetime %3
  /// ```
  /// ->
  /// ```
  ///   ...
  ///   %5 = begin_borrow %nonTrivialField
  ///   use %5, %trivialField
  ///   end_borrow %5
  ///   ...
  ///   %3 = struct $S(%nonTrivialField, %trivialField)
  ///   end_of_lifetime %3
  /// ```
  func simplify(_ context: SimplifyContext) {
    guard ownership == .owned,
          hasOnlyStructExtractUsesInBorrowScopes()
    else {
      return
    }

    for beginBorrow in uses.users(ofType: BeginBorrowInst.self) {
      splitAndRemoveStructExtracts(beginBorrow: beginBorrow, context)
    }

    self.sinkToEndOfLifetime(context)

    context.erase(instructionIncludingAllUsers: self)
  }

  private func hasOnlyStructExtractUsesInBorrowScopes() -> Bool {
    var hasStructExtract = false

    for use in uses.ignoreDebugUses {
      switch use.instruction {
      case let beginBorrow as BeginBorrowInst:
        for borrowUse in beginBorrow.uses.ignoreDebugUses {
          switch borrowUse.instruction {
          case is EndBorrowInst:
            break
          case is StructExtractInst:
            hasStructExtract = true
          default:
            return false
          }
        }
      default:
        guard use.endsLifetime else {
          return false
        }
      }
    }
    return hasStructExtract
  }

  private func splitAndRemoveStructExtracts(beginBorrow: BeginBorrowInst, _ context: SimplifyContext) {
    for structExtract in beginBorrow.uses.users(ofType: StructExtractInst.self) {
      let field = self.operands[structExtract.fieldIndex].value
      switch structExtract.ownership {
      case .none:
        structExtract.replace(with: field, context)
      case .guaranteed:
        let beginBuilder = Builder(before: beginBorrow, context)
        let borrowedField = beginBuilder.createBeginBorrow(of: field,
                                                           isLexical: beginBorrow.isLexical,
                                                           hasPointerEscape: beginBorrow.hasPointerEscape)
        structExtract.replace(with: borrowedField, context)
        for endBorrow in beginBorrow.endInstructions {
          let endBuilder = Builder(before: endBorrow, context)
          endBuilder.createEndBorrow(of: borrowedField)
        }
      case .owned, .unowned:
        fatalError("wrong ownership of struct_extract")
      }
    }
  }
  private func sinkToEndOfLifetime(_ context: SimplifyContext) {
    for use in uses where use.endsLifetime {
      let builder = Builder(before: use.instruction, context)
      let delayedStruct = builder.createStruct(type: type, elements: Array(operands.values))
      use.set(to: delayedStruct, context)
    }
  }
}
