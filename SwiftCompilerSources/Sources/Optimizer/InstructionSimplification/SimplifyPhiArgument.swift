//===--- SimplifyPhiArgument.swift ----------------------------------------===//
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

extension Phi {
  // Triggered from `BranchInst.simplify`
  func simplify(_ context: SimplifyContext) {
    if replacePhiWithIncomingValue(phi: self, context) {
      return
    }
    if replaceReborrowOfBeginBorrows(context) {
      return
    }
    if unwrapAggregate(context) {
      return
    }
  }

  /// If `phi` is a re-borrow phi where all incoming operands are `begin_borrow`s of the same
  /// value, the re-borrow is redundant and can be replaced by a single `begin_borrow` of that
  /// value in the phi's block.
  ///
  /// ```
  ///   bb1:
  ///     %2 = begin_borrow %0
  ///     br bb3(%2)
  ///   bb2:
  ///     %3 = begin_borrow %0
  ///     br bb3(%3)
  ///   bb3(%4 : @reborrow $T):
  ///     %5 = borrowed %4 from (%0)
  ///     // ... uses of %5
  ///     end_borrow %5
  /// ```
  /// ->
  /// ```
  ///   bb1:
  ///     br bb3
  ///   bb2:
  ///     br bb3
  ///   bb3:
  ///     %5 = begin_borrow %0
  ///     // ... uses of %5
  ///     end_borrow %5
  /// ```
  private func replaceReborrowOfBeginBorrows(_ context: SimplifyContext) -> Bool {
    guard isReborrow,
          // All incoming operands must be `begin_borrow`s of the same value. As that value is borrowed
          // in every predecessor, it dominates the phi's block.
          let borrowedValue = getUniqueSourceOfIncomingBeginBorrows()
    else {
      return false
    }

    let block = value.parentBlock

    // Create a new borrow of the common value at the beginning of the phi's block and replace
    // the re-borrow (via its `borrowed_from` user) with it.
    let newBorrow = Builder(atBeginOf: block, context).createBeginBorrow(of: borrowedValue)
    let borrowedFrom = borrowedFrom!
    borrowedFrom.uses.replaceAll(with: newBorrow, context)
    context.erase(instruction: borrowedFrom)

    // Remove the phi operand from all predecessor branches and erase the now-dead incoming
    // `begin_borrow`s.
    erasePhiArgument(phi: self, erasingIncomingInstructions: true, context)
    return true
  }

  private func getUniqueSourceOfIncomingBeginBorrows() -> Value? {
    var borrowedValue: Value? = nil
    for incomingValue in incomingValues {
      guard let beginBorrow = incomingValue as? BeginBorrowInst,
            // The `begin_borrow`'s only purpose must be to feed the re-borrow.
            beginBorrow.uses.singleUse != nil
      else {
        return nil
      }
      if let borrowedValue {
        if beginBorrow.borrowedValue != borrowedValue {
          return nil
        }
      } else {
        borrowedValue = beginBorrow.borrowedValue
      }
    }
    return borrowedValue
  }

  /// "Unwraps" a phi argument if it is an aggregate - a `struct`, `tuple` or `enum` - which is
  /// only used to extract a single element in the phi's block. The "unwrap" is hoisted into all
  /// predecessors, so that the element is passed instead of the whole aggregate.
  ///
  /// ```
  ///   bb1:
  ///     %2 = struct $S (%0)          // %0 : $X
  ///     br bb3(%2)
  ///   bb2:
  ///     br bb3(%1)                   // %1 : $S
  ///   bb3(%4 : @owned $S):
  ///     %5 = begin_borrow %4
  ///     %6 = struct_extract %5, #S.x
  ///     ... // uses of %6
  ///     end_borrow %5
  ///     %7 = destructure_struct %4
  ///     ... // uses of %7
  /// ```
  /// ->
  /// ```
  ///   bb1:
  ///     %2 = struct $S (%0)          // becomes dead after folding with the `destructure_struct`
  ///     %8 = destructure_struct %2
  ///     br bb3(%8)
  ///   bb2:
  ///     %9 = destructure_struct %1
  ///     br bb3(%9)
  ///   bb3(%4 : @owned $X):
  ///     %5 = begin_borrow %4
  ///     ... // uses of %6 replaced by %5
  ///     end_borrow %5
  ///     ... // uses of %7 replaced by %4
  /// ```
  /// If an incoming value is a matching aggregate instruction - like `%2` above - the redundant
  /// instruction pair is removed by other simplifications. But this is not a pre-condition:
  /// hoisting the "unwrap" is beneficial on its own, because the phi passes a smaller value.
  ///
  /// As shown in this example, the "unwrap" instructions can also be nested in borrow scopes
  /// of the phi.
  private func unwrapAggregate(_ context: SimplifyContext) -> Bool {
    // The incoming values of a re-borrow phi must be borrow-introducing instructions.
    // A hoisted "unwrap" is a guaranteed forwarding instruction, which is not allowed here.
    if isReborrow {
      return false
    }

    var uniqueUnwrap: Unwrap? = nil
    guard collectUnwraps(of: borrowedFrom ?? value, into: &uniqueUnwrap),
          let uniqueUnwrap
    else {
      return false
    }
    let elementType = uniqueUnwrap.element.type

    // Hoist the "unwrap" into all predecessors: pass the element instead of the aggregate.
    for incomingOp in incomingOperands {
      let element = uniqueUnwrap.createHoistedUnwrap(of: incomingOp.value, resultType: elementType,
                                                     before: incomingOp.instruction, context)
      incomingOp.set(to: element, context)
    }

    // The borrowed-from instruction is re-created by `updateGuaranteedPhis` below.
    if let borrowedFrom {
      borrowedFrom.replace(with: value, context)
    }

    let isTrivialElement = elementType.isTrivial(in: value.parentFunction)
    let newOwnership: Ownership = isTrivialElement ? .none : value.ownership
    let newArgument = value.parentBlock.replacePhiArgumentAndReplaceAllUses(
      at: value.index, type: elementType, ownership: newOwnership, context)

    if isTrivialElement {
      // A trivial value cannot be borrowed. Therefore the borrow scopes of the phi are not
      // needed anymore.
      removeBorrowScopes(of: newArgument, context)
    }
    removeUnwraps(of: newArgument, context)

    updateGuaranteedPhis(phis: [Phi(newArgument)!], context)
    return true
  }
}

/// An instruction which extracts a single element from an aggregate value.
private struct Unwrap {
  let instruction: Instruction

  /// The index of the extracted struct field or tuple element. For `enum`s it's the case index.
  let elementIndex: Int

  init?(_ instruction: Instruction) {
    switch instruction {
    case let structExtract as StructExtractInst:
      self.elementIndex = structExtract.fieldIndex
    case let tupleExtract as TupleExtractInst:
      self.elementIndex = tupleExtract.fieldIndex
    case let enumData as UncheckedEnumDataInst:
      self.elementIndex = enumData.caseIndex
    case is DestructureStructInst, is DestructureTupleInst:
      // A `destructure` extracts all elements at once. Therefore it's only an "unwrap" if a
      // single element is actually used.
      var usedElementIndex: Int? = nil
      for (index, result) in instruction.results.enumerated() where !result.uses.isEmpty {
        if usedElementIndex != nil {
          return nil
        }
        usedElementIndex = index
      }
      guard let usedElementIndex else {
        return nil
      }
      self.elementIndex = usedElementIndex
    default:
      return nil
    }
    self.instruction = instruction
  }

  /// The aggregate from which the element is extracted.
  var aggregate: Value { instruction.operands[0].value }

  /// The extracted element.
  var element: Value {
    if let singleValueInst = instruction as? SingleValueInstruction {
      return singleValueInst
    }
    return instruction.results[elementIndex]
  }

  /// Creates an instruction which extracts the same element as this unwrap - but from `aggregate` -
  /// and returns the extracted element.
  ///
  /// If `aggregate` is owned it must be consumed by the new instruction, which means that a
  /// `destructure` is needed for structs and tuples. All the other - not extracted - elements are
  /// destroyed right away.
  func createHoistedUnwrap(of aggregate: Value, resultType: Type,
                           before insertionPoint: Instruction, _ context: SimplifyContext) -> Value {
    let builder = Builder(before: insertionPoint, context)
    let isConsuming = aggregate.ownership == .owned

    switch instruction {
    case is StructExtractInst, is DestructureStructInst:
      if !isConsuming {
        return builder.createStructExtract(struct: aggregate, fieldIndex: elementIndex)
      }
      return builder.createDestructureStruct(struct: aggregate).results[elementIndex]
    case is TupleExtractInst, is DestructureTupleInst:
      if !isConsuming {
        return builder.createTupleExtract(tuple: aggregate, elementIndex: elementIndex)
      }
      return builder.createDestructureTuple(tuple: aggregate).results[elementIndex]
    case is UncheckedEnumDataInst:
      // `unchecked_enum_data` forwards the ownership of the enum and there is only a single
      // payload element. Therefore it works for owned and guaranteed enums.
      return builder.createUncheckedEnumData(enum: aggregate, caseIndex: elementIndex,
                                             resultType: resultType)
    default:
      fatalError("unhandled unwrap instruction")
    }
  }
}

/// Collects all instructions which extract an element from `aggregate`, including instructions
/// which extract from a borrow scope of `aggregate`.
/// Returns false if `aggregate` has any other uses.
private func collectUnwraps(of aggregate: Value, into uniqueUnwrap: inout Unwrap?) -> Bool {
  for use in aggregate.uses {
    switch use.instruction {
    case let beginBorrow as BeginBorrowInst:
      if !collectUnwraps(of: beginBorrow, into: &uniqueUnwrap) {
        return false
      }
    case is EndBorrowInst:
      break
    default:
      guard use.index == 0, let unwrap = Unwrap(use.instruction) else {
        return false
      }
      if let uniqueUnwrap {
        if unwrap.elementIndex != uniqueUnwrap.elementIndex {
          return false
        }
      } else {
        uniqueUnwrap = unwrap
      }
    }
  }
  return true
}

/// Removes all borrow scopes of `value`, which cannot be borrowed anymore because it became
/// trivial.
private func removeBorrowScopes(of value: Value, _ context: SimplifyContext) {
  for use in value.uses {
    if let beginBorrow = use.instruction as? BeginBorrowInst {
      // Remove nested borrow scopes first, so that all their uses end up at `beginBorrow`.
      removeBorrowScopes(of: beginBorrow, context)
      context.erase(instructions: beginBorrow.uses.users(ofType: EndBorrowInst.self))
      beginBorrow.replace(with: value, context)
    }
  }
}

/// Removes all unwrap instructions of `value`, which is not an aggregate anymore, but the
/// unwrapped element itself. Therefore all uses of an unwrap can be replaced by its operand.
///
/// Borrow scopes of `value` need to be re-created, because the result type of an existing
/// `begin_borrow` still refers to the original aggregate type.
private func removeUnwraps(of value: Value, _ context: SimplifyContext) {
  for use in value.uses {
    switch use.instruction {
    case let beginBorrow as BeginBorrowInst:
      let newBorrow = Builder(before: beginBorrow, context).createBeginBorrow(
        of: value,
        isLexical: beginBorrow.isLexical,
        hasPointerEscape: beginBorrow.hasPointerEscape,
        isFromVarDecl: beginBorrow.isFromVarDecl)
      beginBorrow.replace(with: newBorrow, context)
      removeUnwraps(of: newBorrow, context)
    case is EndBorrowInst:
      break
    default:
      let unwrap = Unwrap(use.instruction)!
      unwrap.element.uses.replaceAll(with: value, context)
      context.erase(instruction: unwrap.instruction)
    }
  }
}
