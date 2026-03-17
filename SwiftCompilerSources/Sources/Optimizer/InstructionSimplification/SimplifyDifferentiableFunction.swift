//===--- SimplifyDifferentiableFunction.swift -----------------------------===//
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

extension DifferentiableFunctionInst : SILCombineSimplifiable {

  /// Eliminates `differentiable_function_extract`s of an owned `differentiable_function` where the
  /// `differentiable_function_extract`s are inside a borrow scope.
  /// This is done by splitting the `begin_borrow` of the whole `differentiable_function` into individual borrows of the extractees
  /// (for trivial extractees no borrow is needed). If needed, `convert_function` is emitted to cast the extractee to the
  /// ABI-compatible expected type.
  ///
  /// ```
  ///   %3 = differentiable_function [parameters X] [results X] %orig with_derivative { %jvp, %vjp }
  ///   ...
  ///   %4 = begin_borrow %3
  ///   %5 = differentiable_function_extract [original] %4
  ///   %6 = differentiable_function_extract [jvp] %4
  ///   %7 = differentiable_function_extract [vjp] %4
  ///   use %5, %6, %7
  ///   end_borrow %4
  ///   ...
  ///   end_of_lifetime %3
  /// ```
  /// ->
  /// ```
  ///   %3 = differentiable_function [parameters X] [results X] %orig with_derivative { %jvp, %vjp }
  ///   ...
  ///   %5  = begin_borrow %orig
  ///   %5x = convert_function %5 to XXX
  ///   use %5x
  ///   end_borrow %5
  ///   ...
  ///   %6  = begin_borrow %jvp
  ///   %6x = convert_function %6 to XXX
  ///   use %6x
  ///   end_borrow %6
  ///   ...
  ///   %7  = begin_borrow %vjp
  ///   %7x = convert_function %7 to XXX
  ///   use %7x
  ///   end_borrow %7
  ///   ...
  ///   end_of_lifetime %3
  /// ```
  func simplify(_ context: SimplifyContext) {
    guard ownership == .owned,
          hasOnlyExtractUsesInBorrowScopes()
    else {
      return
    }

    for use in uses {
      switch use.instruction {
      case let beginBorrow as BeginBorrowInst:
        splitAndRemoveExtracts(beginBorrow: beginBorrow, context)
      case is DebugValueInst:
        break
      default:
        assert(use.endsLifetime)
      }
    }
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
          case is DifferentiableFunctionExtractInst:
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
    for differentiableFunctionExtract in beginBorrow.uses.users(ofType: DifferentiableFunctionExtractInst.self) {
      guard let extractee = self.getExtractee(extractee: differentiableFunctionExtract.extractee) else {
        continue
      }

      switch differentiableFunctionExtract.ownership {
      case .none:
        if differentiableFunctionExtract.type != extractee.type {
          let convertBuilder = Builder(before: differentiableFunctionExtract, context)
          let newField = convertBuilder.createConvertFunction(originalFunction: extractee, resultType: differentiableFunctionExtract.type, withoutActuallyEscaping: false)
          differentiableFunctionExtract.replace(with: newField, context)
        } else {
          differentiableFunctionExtract.replace(with: extractee, context)
        }

      case .guaranteed:
        let beginBuilder = Builder(before: beginBorrow, context)
        let borrowedField = beginBuilder.createBeginBorrow(of: extractee,
                                                           isLexical: beginBorrow.isLexical,
                                                           hasPointerEscape: beginBorrow.hasPointerEscape)
        if differentiableFunctionExtract.type != extractee.type {
          let convertBuilder = Builder(before: differentiableFunctionExtract, context)
          let newField = convertBuilder.createConvertFunction(originalFunction: borrowedField, resultType: differentiableFunctionExtract.type, withoutActuallyEscaping: false)
          differentiableFunctionExtract.replace(with: newField, context)
        } else {
          differentiableFunctionExtract.replace(with: borrowedField, context)
        }
        for endBorrow in beginBorrow.endInstructions {
          let endBuilder = Builder(before: endBorrow, context)
          endBuilder.createEndBorrow(of: borrowedField)
        }

      case .owned, .unowned:
        fatalError("wrong ownership of differentiable_function_extract")
      }
    }
  }
}
