//===--- SimplifyDebugReconstruction.swift --------------------------------===//
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

/// Instructions whose only reconstruction block simplification, besides the C++ simplifications
/// which apply to every instruction, is folding an `undef` operand.
///
/// These are all salvageable into a debug reconstruction block (see `salvageDebugInfo`), but
/// have nothing to simplify in a regular block.
protocol UndefFoldingSimplifiable : DebugReconstructionBlockSimplifiable, SingleValueInstruction {
}

extension UndefFoldingSimplifiable {
  func simplifyForDebugReconstructionBlock(_ context: SimplifyContext) {
    foldUndefOperands(context)
  }
}

// Note: `enum` is deliberately not listed here, as folding away a known case loses information.

// Casts and conversions.
extension ConvertFunctionInst : UndefFoldingSimplifiable {}

// Address projections.
extension StructElementAddrInst : UndefFoldingSimplifiable {}
extension TupleElementAddrInst : UndefFoldingSimplifiable {}
extension RefElementAddrInst : UndefFoldingSimplifiable {}
extension RefTailAddrInst : UndefFoldingSimplifiable {}
extension VectorBaseAddrInst : UndefFoldingSimplifiable {}
extension UncheckedInPlaceEnumDataAddrInst : UndefFoldingSimplifiable {}
extension AddressToPointerInst : UndefFoldingSimplifiable {}

extension UpcastInst : DebugReconstructionBlockSimplifiable {
  /// Folds undef, and combines a chain of upcasts:
  /// ```
  ///   %1 = upcast %0 : $Derived to $Base
  ///   %2 = upcast %1 : $Base to $Root   ->   %2 = upcast %0 : $Derived to $Root
  /// ```
  func simplifyForDebugReconstructionBlock(_ context: SimplifyContext) {
    if foldUndefOperands(context) {
      return
    }
    while let upcast = fromInstance as? UpcastInst {
      operand.set(to: upcast.fromInstance, context)
    }
  }
}
