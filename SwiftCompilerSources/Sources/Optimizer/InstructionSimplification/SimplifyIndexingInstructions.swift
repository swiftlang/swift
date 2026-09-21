//===--- SimplifyIndexingInstructions.swift -------------------------------===//
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

extension IndexAddrInst : Simplifiable, SILCombineSimplifiable {

  func simplify(_ context: SimplifyContext) {
    if tryRemoveZeroIndex(context) {
      return
    }

    _ = tryFoldNestedIndexAddrs(context)
  }

  /// Removes a zero-index `index_addr` by replacing it with its base:
  /// ```
  ///   %1 = index_addr %base, 0
  /// ```
  /// ->
  /// ```
  ///   // uses of %1 replaced with %base
  /// ```
  ///
  private func tryRemoveZeroIndex(_ context: SimplifyContext) -> Bool {
    // `index_addr [projection]` must not be removed, even with a zero index.
    guard !isProjection,
          let indexValue = constantIndex,
          indexValue == 0
    else {
      return false
    }

    self.replace(with: base, context)
    return true
  }

  /// Fold nested `index_addr` instructions with constant indices:
  /// ```
  ///   %1 = index_addr %ptr, x   // outer
  ///   %2 = index_addr %1, y     // self
  /// ```
  /// ->
  /// ```
  ///   %2 = index_addr %ptr, x+y
  /// ```
  ///
  private func tryFoldNestedIndexAddrs(_ context: SimplifyContext) -> Bool {
    guard let indexValue = constantIndex,
          let outer = base as? IndexAddrInst,
          let outerIndexValue = outer.constantIndex
    else {
      return false
    }

    let (combinedIndex, overflow) = indexValue.addingReportingOverflow(outerIndexValue)
    if overflow {
      return false
    }

    let builder = Builder(before: self, context)
    let newIndexLiteral = builder.createIntegerLiteral(combinedIndex, type: index.type)
    let newIndexAddr = builder.createIndexAddr(
      base: outer.base,
      index: newIndexLiteral,
      needStackProtection: needsStackProtection || outer.needsStackProtection,
      isProjection: isProjection
    )
    self.replace(with: newIndexAddr, context)

    return true
  }
}

extension IndexRawPointerInst : Simplifiable, SILCombineSimplifiable {

  func simplify(_ context: SimplifyContext) {
    if tryRemoveZeroIndex(context) {
      return
    }

    _ = tryFoldNestedIndexRawPointers(context)
  }

  /// Removes a zero-index `index_raw_pointer` by replacing it with its base:
  /// ```
  ///   %1 = index_raw_pointer %base, 0
  /// ```
  /// ->
  /// ```
  ///   // uses of %1 replaced with %base
  /// ```
  private func tryRemoveZeroIndex(_ context: SimplifyContext) -> Bool {
    guard let indexValue = constantIndex,
          indexValue == 0
    else {
      return false
    }

    self.replace(with: base, context)
    return true
  }

  /// Fold nested `index_raw_pointer` instructions with constant indices:
  /// ```
  ///   %1 = index_raw_pointer %ptr, x   // outer
  ///   %2 = index_raw_pointer %1, y     // self
  /// ```
  /// ->
  /// ```
  ///   %2 = index_raw_pointer %ptr, x+y
  /// ```
  ///
  private func tryFoldNestedIndexRawPointers(_ context: SimplifyContext) -> Bool {
    guard let indexValue = constantIndex,
          let outer = base as? IndexRawPointerInst,
          let outerIndexValue = outer.constantIndex
    else {
      return false
    }

    let (combinedIndex, overflow) = indexValue.addingReportingOverflow(outerIndexValue)
    if overflow {
      return false
    }

    let builder = Builder(before: self, context)
    let newIndexLiteral = builder.createIntegerLiteral(combinedIndex, type: index.type)
    let newIndexRawPointer = builder.createIndexRawPointer(
      base: outer.base,
      index: newIndexLiteral
    )
    self.replace(with: newIndexRawPointer, context)

    return true
  }
}
