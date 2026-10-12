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
  ///
  /// Similarly, split `destructure_struct` of copies:
  /// ```
  ///   %3 = struct $S(%1, %2)  // owned
  ///   ...
  ///   %4 = copy_value %3
  ///   (%5, %6) = destructure_struct %5
  /// ```
  /// ->
  /// ```
  ///   ...
  ///   %5 = copy_value %1
  ///   %6 = copy_value %2
  /// ```
  func simplify(_ context: SimplifyContext) {
    splitOwnedAggregate(context)
  }
}

/// The common implementation for `struct` and `tuple` instructions.
extension SingleValueInstruction {
  /// Eliminates `struct_extract`s/`tuple_extract`s of an owned `struct`/`tuple` where the extracts are
  /// inside a borrow scope. See the comment of `StructInst.simplify`.
  func splitOwnedAggregate(_ context: SimplifyContext) {
    guard ownership == .owned,
          hasOnlyExtractUsesInBorrowScopes()
    else {
      return
    }

    for use in uses {
      switch use.instruction {
      case let beginBorrow as BeginBorrowInst:
        splitAndRemoveExtracts(beginBorrow: beginBorrow, context)
      case let copy as CopyValueInst:
        splitAndRemoveDestructuresOfCopy(copy: copy, context)
      case is DebugValueInst:
        break
      default:
        assert(use.endsLifetime)
        sinkToEndOfLifetime(use: use, context)
      }
    }
    context.erase(instructionIncludingAllUsers: self)
  }

  private func hasOnlyExtractUsesInBorrowScopes() -> Bool {
    var hasExtract = false

    for use in uses.ignoreDebugUses {
      switch use.instruction {
      case let beginBorrow as BeginBorrowInst:
        for borrowUse in beginBorrow.uses.ignoreDebugUses {
          switch borrowUse.instruction {
          case is EndBorrowInst:
            break
          case is StructExtractInst, is TupleExtractInst:
            hasExtract = true
          default:
            return false
          }
        }
      case let copy as CopyValueInst:
        for copyUse in copy.uses.ignoreDebugUses {
          switch copyUse.instruction {
          case is DestructureStructInst, is DestructureTupleInst:
            hasExtract = true
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
    return hasExtract
  }

  private func splitAndRemoveExtracts(beginBorrow: BeginBorrowInst, _ context: SimplifyContext) {
    for extract in beginBorrow.uses.users(ofType: SingleValueInstruction.self) {
      let fieldIndex: Int
      switch extract {
      case let structExtract as StructExtractInst: fieldIndex = structExtract.fieldIndex
      case let tupleExtract as TupleExtractInst:   fieldIndex = tupleExtract.fieldIndex
      default:                                     continue
      }
      let field = self.operands[fieldIndex].value
      switch extract.ownership {
      case .none:
        extract.replace(with: field, context)
      case .guaranteed:
        let beginBuilder = Builder(before: beginBorrow, context)
        let borrowedField = beginBuilder.createBeginBorrow(of: field,
                                                           isLexical: beginBorrow.isLexical,
                                                           hasPointerEscape: beginBorrow.hasPointerEscape)
        extract.replace(with: borrowedField, context)
        for endBorrow in beginBorrow.endInstructions {
          let endBuilder = Builder(before: endBorrow, context)
          endBuilder.createEndBorrow(of: borrowedField)
        }
      case .owned, .unowned:
        fatalError("wrong ownership of struct_extract/tuple_extract")
      }
    }
  }

  /// ```
  ///   %4 = copy_value %3
  ///   (%5, %6) = destructure_struct %5
  /// ```
  /// ->
  /// ```
  ///   %5 = copy_value %1
  ///   %6 = copy_value %2
  /// ```
  private func splitAndRemoveDestructuresOfCopy(copy: CopyValueInst, _ context: SimplifyContext) {
    for (fieldIndex, field) in self.operands.values.enumerated() {
      let copiedField = if field.ownership == .none {
        field
      } else {
        Builder(before: copy, context).createCopyValue(operand: field)
      }
      for destructure in copy.uses.users(ofType: MultipleValueInstruction.self) {
        destructure.results[fieldIndex].uses.replaceAll(with: copiedField, context)
      }
    }
    context.erase(instructionIncludingAllUsers: copy)
  }

  private func sinkToEndOfLifetime(use: Operand, _ context: SimplifyContext) {
    let builder = Builder(before: use.instruction, context)
    let delayedAggregate: Value
    switch self {
    case is StructInst:
      delayedAggregate = builder.createStruct(type: type, elements: Array(operands.values))
    case is TupleInst:
      delayedAggregate = builder.createTuple(type: type, elements: Array(operands.values))
    default:
      fatalError("unexpected aggregate instruction")
    }
    use.set(to: delayedAggregate, context)
  }
}
